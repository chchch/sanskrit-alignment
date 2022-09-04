import { Sanscript } from './sanscript.mjs';
import { showSaveFilePicker } from 'https://cdn.jsdelivr.net/npm/native-file-system-adapter/mod.js';
//import { showSaveFilePicker } from 'native-file-system-adapter';

const Exporter = function(Utils,Xslt) {
    const Find = Utils.find;
    const Make = Utils.make;
    const TeiNS = Find.teins();

    const exp = {
        write: async function(file,handle) {
            const writer = await handle.createWritable();
            writer.write(file);
            writer.close();
        },

        xml: async function(doc) {
            const str = new XMLSerializer().serializeToString(
                Xslt.sheets['xml'].transformToDocument(doc)
            );
            const file = new Blob([str], {type: 'text/xml;charset=utf-8'});
            const fileURL = Find.basename() + '.xml';
            const fileHandle = await showSaveFilePicker({
                _preferPolyfill: false,
                suggestedName: fileURL,
                types: [ {description: 'TEI XML', accept: {'text/xml': ['.xml']} } ],
            });
            exp.write(file,fileHandle);
        },

        csv: async function(doc) {
            const str = Xslt.sheets['csv'].transformToDocument(doc).documentElement.textContent;
            const file = new Blob([str], {type: 'text/csv;charset=utf-8'});
            const fileURL = Find.basename() + '.csv';
            const fileHandle = await showSaveFilePicker({
                _preferPolyfill: false,
                suggestedName: fileURL,
                types: [ {description: 'CSV file', accept: {'text/csv': ['.csv']} } ],
            });
            exp.write(file,fileHandle);
        },
  
        nexus: async function(doc) {
            const texts = [...Find.texts(doc)];
            const ntax = texts.length;
            const symbols = '0 1 2 3 4 5 6 7 8 9 A B C D E F G H K L M N P Q R S T U V W X Y Z a b c d e f g h k l m n p q r s t u v w x y z';
            const [siggap,...symbolarr] = symbols.split(' ');
            const gap = '-';
            const taxlabels = texts.map(el => '\''+el.parentElement.getAttribute('n')+'\'');
            const textWalkers = texts.map(el => Find.textWalker(el));
            const nchar = texts[0].querySelectorAll('w').length;
            const charstatelabels = [];
            const matrix = taxlabels.map(s => [s + ' ']);
            for(let n=0;n<nchar;n++) {
                const statelabels = new Set();
                const readings = [];
                for(let m=0;m<textWalkers.length;m++) {
                    const walker = textWalkers[m];
                    const node = walker.nextNode();
                    const reading = Sanscript.t(node.textContent.trim().toLowerCase(),'iast','hk')
                        .replace(/'/g,'()');
                    readings.push(reading);
                    if(reading !== '' && reading !== '[]')
                        statelabels.add(reading);
                }
                charstatelabels.push(['[]',...statelabels]);
                const statesymbols = new Map([...statelabels].map((x,i) => [x,symbolarr[i]]));
                for(let p=0;p<readings.length;p++) {
                    const r = ((p) => {
                        switch (readings[p]) {
                        case '':
                            return gap;
                        case '[]':
                            return siggap;
                        default:
                            return statesymbols.get(readings[p]);
                        }
                    })(p);

                    matrix[p].push(r);
                }
            }
            const charstatestr = charstatelabels.map((x,i) => (i+1) +' / '+ [...x].map(s => `'${s}'`).join(' ')).join(',\n');
            const flatmatrix = matrix.map(arr => arr.join(''))
                // ignore long gaps, even if "gaps are significant" is selected
                .map(str => str.replace(/0{8,}/g, match => match.replace(/0/g,'-')))
                .reduce((acc,cur) => acc + '\n'+cur);
            const str =
`#NEXUS

BEGIN TAXA;
  DIMENSIONS NTAX=${ntax};
  TAXLABELS ${taxlabels.join(' ')};
END;

BEGIN CHARACTERS;
  DIMENSIONS
    NCHAR=${nchar};
  FORMAT 
    DATATYPE=STANDARD 
    RESPECTCASE 
    GAP=${gap} 
    MISSING=? 
    SYMBOLS="${symbols}";
  CHARSTATELABELS
${charstatestr}
;
MATRIX
${flatmatrix}
;
END;
`;
            const file = new Blob([str], {type: 'text/nexus;charset=iso8859-1'});
            const fileURL = Find.basename() + '.nex';
            const fileHandle = await showSaveFilePicker({
                _preferPolyfill: false,
                suggestedName: fileURL,
                types: [ {description: 'NEXUS file', accept: {'text/nexus': ['.nex']} } ],
            });
            exp.write(file,fileHandle);
        },

        processOptions: function(opts) {
            const doc = Find.curxml().cloneNode(true);

            if(opts.get('option_normalize')) {
                const els = doc.querySelectorAll('w[lemma]');
                for(const el of els)
                    el.textContent = el.getAttribute('lemma');
            }

            if(opts.get('option_mergegroups')) {
                const els = doc.querySelectorAll('cl');
                for(const el of els) {
                    const firstw = el.removeChild(el.firstChild);
                    while(el.firstChild) {
                        const norm1 = firstw.getAttribute('lemma') || firstw.textContent;
                        const norm2 = el.firstChild.getAttribute('lemma') || el.firstChild.textContent;
                        firstw.setAttribute('lemma',norm1 + norm2);
                        while(el.firstChild.firstChild)
                            firstw.appendChild(el.firstChild.firstChild);
                        el.firstChild.remove();
                    }
                    if(firstw.getAttribute('lemma') === firstw.textContent)
                        firstw.removeAttribute('lemma');
                    el.parentNode.insertBefore(firstw,el);
                    el.parentNode.removeChild(el);
                }
            }

            if(opts.get('option_insignificant')) {
                const els = doc.querySelectorAll('w[insignificant="true"]');
                for(const el of els)
                    el.parentNode.removeChild(el);
            }
            if(opts.get('option_binary')) {
                /*
                const els = doc.querySelectorAll('w[binary="true"]');
                const nonempty = [...els].filter(el => el.textContent !== '');
                if(!nonempty) return;
                
                const placeholder = '['+nonempty[0].textContent+']';
                for(const el of nonempty)
                    el.textContent = placeholder;
                */
                const firstrow = Find.firsttext().querySelectorAll('w[binary="true"]');
                //const allels = [...doc.querySelectorAll('w[binary="true"]')];
                const nums = [...firstrow].map(el => el.getAttribute('n'));
                for(const num of nums) {
                    const nonempty = doc.querySelectorAll(`w[n="${num}"]:not(:empty)`);
                    //const nonempty = [...els].filter(el => el.childNodes.length !== 0);
                    /*
                    const nonempty = allels.filter(el => {
                        return (el.getAttribute('n') === num &&
                                el.childNodes.length === 0);
                    });
                    */
                    if(nonempty.length === 0) continue;
                    // Element nodes have no nodeValue
                    const placeholder = `[${nonempty[0].textContent}]`;
                    for(const el of nonempty)
                        el.textContent = placeholder;
                }
            }
            if(opts.get('option_noempty')) {
                const els = doc.querySelectorAll('w:empty');
                for(const el of els)
                    el.textContent = '[]';
            }

            // early return for no apparatus
            if(!opts.get('option_basetext')) 
                return doc;
            else
                return exp.makeApp(opts,doc);
        },
        makeHeader(newdoc,doc) {
            const teiheader = newdoc.createElementNS(TeiNS,'teiHeader');
            const filedesc = newdoc.createElementNS(TeiNS,'fileDesc');
            const sourcedesc = newdoc.createElementNS(TeiNS,'sourceDesc');
            
            const msdesc = newdoc.createElementNS(TeiNS,'msDesc');
            const msid = newdoc.createElementNS(TeiNS,'msIdentifier');
            const siglum = newdoc.createElementNS(TeiNS,'abbr');
            siglum.setAttribute('type','siglum');
            siglum.append(newdoc.documentElement.getAttribute('n'));
            msid.appendChild(siglum);
            msdesc.appendChild(msid);
            sourcedesc.appendChild(msdesc);

            const listwit = newdoc.createElementNS(TeiNS,'listWit');
            for(const text of Find.teis(doc)) {
                const n = text.getAttribute('n');
                const wit = newdoc.createElementNS(TeiNS,'witness');
                wit.setAttribute('xml:id',n);
                const abbr = newdoc.createElementNS(TeiNS,'abbr');
                abbr.setAttribute('type','siglum');
                abbr.append(n);
                wit.appendChild(abbr);
                listwit.appendChild(wit);
            }
            sourcedesc.appendChild(listwit);
            filedesc.appendChild(sourcedesc);
            teiheader.appendChild(filedesc);
            return teiheader;
        },
        makeApp(opts,doc) {
            const baselabel = opts.get('option_basetext');
            const basetext = Find.tei(baselabel,doc);
            const normlem = opts.get('option_app_normalize');

            const newdoc = document.implementation.createDocument(TeiNS,'',null);

            newdoc.appendChild(basetext);

            // add basic text structure
            const bod = newdoc.createElementNS(TeiNS,'body');
            const par = newdoc.createElementNS(TeiNS,'p');
            
            const oldtext = basetext.querySelector('text');
            while(oldtext.firstChild)
                par.appendChild(oldtext.firstChild);
            bod.appendChild(par);
            oldtext.appendChild(bod);

            const teiheader = exp.makeHeader(newdoc,doc);
            basetext.insertBefore(teiheader,basetext.firstChild);

            const siglum = newdoc.documentElement.getAttribute('n');
            const words = Find.words(false,basetext);
            for(const word of words) {
                const dataN = word.getAttribute('n');
                const lemma = normlem ? 
                    (word.getAttribute('lemma') || word.textContent) :
                    word.textContent;
                const posapp = [siglum];
                const negapp = new Map();
                const otherwords = Find.words(dataN,doc);
                for(const otherword of otherwords) {
                    const id = otherword.closest('TEI').getAttribute('n');
                    if(normlem && otherword.getAttribute('lemma') === lemma)
                        posapp.push(id);
                    else if(otherword.textContent === lemma)
                        posapp.push(id);
                    else {
                        const newstr = otherword.textContent === '' ? 
                            '[om.]' : 
                            otherword.textContent;
                        const negwits = negapp.get(newstr) || [];
                        negwits.push(id);
                        negapp.set(newstr,negwits);
                    }
                }
                
                if(negapp.size === 0) {
                    while(word.firstChild)
                        word.parentElement.insertBefore(word.firstChild,word);
                }
                else {
                    const app = newdoc.createElementNS(TeiNS,'app');
                    app.setAttribute('n',dataN);
                    const lem = newdoc.createElementNS(TeiNS,'lem');
                    const poswits = posapp.map(s => `#${s}`).join(' ');
                    if(poswits !== '') lem.setAttribute('wit',poswits);
                    while(word.firstChild)
                        lem.appendChild(word.firstChild);
                    app.appendChild(lem);
                    for(const [str,wits] of negapp) {
                        const rdg = newdoc.createElementNS(TeiNS,'rdg');
                        const negwits = wits.map(s => `#${s}`).join(' ');
                        rdg.setAttribute('wit',negwits);
                        rdg.append(str);
                        app.appendChild(rdg);
                    }
                    word.parentElement.insertBefore(app,word);
                    if(lem.textContent.match(/\s$/)) {
                        word.parentElement.insertBefore(
                            newdoc.createTextNode(' '),
                            word);
                        lem.textContent = lem.textContent.trimEnd();
                    }
                }

                word.remove();
            }
            return newdoc;
        },

        options() {
            return document.createRange().createContextualFragment(
                `<div id="exportoptions" class="popup">
    <form id="exportform">
      <div style="font-weight: bold">Export</div>
      <div>
        <input type="checkbox" id="option_insignificant" name="option_insignificant"><label for="option_insignificant">Remove insignifiant lemmata</label>
      </div>
      <div>
        <input type="checkbox" id="option_binary" name="option_binary"><label for="option_binary">Binarize marked lemmata</label>
      </div>
      <div>
        <input type="checkbox" id="option_noempty" name="option_noempty"><label for="option_noempty">Gaps are significant</label>
      </div>
      <div>
        <input type="checkbox" id="option_mergegroups" name="option_mergegroups"><label for="option_mergegroups">Merge groups</label>
      </div>
      <div>
        <input type="checkbox" id="option_normalize" name="option_normalize"><label for="option_normalize">Normalize spellings</label>
      </div>
      <div>
        <button type="submit">Export</button>
      </div>
    </form>
</div>`);
        },
        appOptions() {
            const frag = document.createRange().createContextualFragment(
                `<div id="exportoptions" class="popup">
    <form id="exportform">
      <div style="font-weight: bold">Export</div>
      <div>
        <label for="option__basetext">Base text</label>
        <select id="option__basetext" name="option_basetext"></select>
      </div>
      <div>
        <input type="checkbox" id="option__app_insignificant" name="option_app_insignificant"><label for="option_app_insignificant">Ignore insignifiant lemmata</label>
      </div>
      <div>
        <input type="checkbox" id="option_mergegroups" name="option_mergegroups" checked><label for="option_mergegroups">Merge groups</label>
      </div>
      <div>
        <input type="checkbox" id="option_app_normalize" name="option_app_normalize" checked><label for="option_app_normalize">Normalized spellings in positive apparatus</label>
      </div>
      <div>
        <input type="checkbox" id="option_normalize" name="option_normalize"><label for="option_normalize">Normalize spellings in negative apparatus</label>
      </div>
      <div>
        <button type="submit">Export</button>
      </div>
    </form>
</div>`);
            const select = frag.getElementById('option__basetext');
            const sigla = [...Find.trs()].map(el => el.dataset.n);
            for(const siglum of sigla) {
                const el = document.createElement('option');
                el.value = siglum;
                el.append(siglum);
                select.appendChild(el);
            }
            return frag;
        },

        showOptions: function(func,optfunc) {
            const htmlfrag = optfunc();

            const submitfunction = function(e) {
                e.preventDefault();
                var opts = [];
                const inputs = document.getElementById('exportform').elements;
                for(const i of inputs) {
                    if(i.type === 'checkbox')
                        opts.push([i.name,i.checked]);
                    else
                        opts.push([i.name,i.value]);
                }
                const doc = exp.processOptions(new Map(opts));
                func(doc);
            };
            
            Make.blackout(htmlfrag,submitfunction);
        },
/*
        showOptions: function(func,optfunc) {
            const htmlfrag = optfunc();
            const blackout = document.createElement('div');
            blackout.id = 'blackout';
            blackout.appendChild(htmlfrag);
            document.body.appendChild(blackout);
            const submitfunction = function(e) {
                e.preventDefault();
                var opts = [];
                const inputs = document.getElementById('exportform').elements;
                for(const i of inputs) {
                    if(i.type === 'checkbox')
                        opts.push([i.name,i.checked]);
                    else
                        opts.push([i.name,i.value]);
                }
                const doc = exp.processOptions(new Map(opts));
                func(doc);
                document.body.removeChild(blackout);
            };
            const submit = blackout.querySelector('button');
            submit.addEventListener('click',submitfunction);
            blackout.addEventListener('click',exp.blackoutClick);
            return blackout;
        },

        blackoutClick: function(e) {
            const targ = e.target.closest('.popup');
            if(!targ) {
                const blackout = document.querySelector('#blackout');
                blackout.parentNode.removeChild(blackout);
            }
        }
*/   
    };

    return exp;
}

export { Exporter };
