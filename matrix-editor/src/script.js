import { Sanscript } from './lib/sanscript.mjs';
import { CSV } from './lib/csv.mjs';
import { Smits } from './lib/jsphylosvg-custom.mjs';

import { showSaveFilePicker } from 'https://cdn.jsdelivr.net/npm/native-file-system-adapter/mod.js';
//import { showSaveFilePicker } from 'native-file-system-adapter';

import { Normalizer } from './lib/normalize.mjs';
import { changeScript } from './lib/transliterate.mjs';
import { xslt as _Xslt } from './lib/xslt.mjs';
import { find as _Find } from './lib/find.mjs';
import { check as _Check } from './lib/check.mjs';
import { workerFunc } from './lib/worker.mjs';

import _Hypher from 'hypher';
import { hyphenation_sa } from './lib/hypher-sa.mjs';

'use strict';

window.comboView = (function() {
    
    const _const = {
        teins: 'http://www.tei-c.org/ns/1.0',
        scripts: ['iast','devanagari','telugu','grantha','malayalam']
    };

    const _state = {
        filename: null,
        xml: null,
        treelist: new Map(),
        trees: [],
        textboxes: [],
        matrix: null,
        viewdiv: null,
        descs: null,
        maxlemma: null,
        windows: [window],
        dragged: null,
        undo: [],
        redo: [],
        editing: false
    };

    const Hypher = new _Hypher(hyphenation_sa);
    const Xslt = new _Xslt(_state, _const);
    const Find = new _Find(_state);
    const Check = new _Check(_state, Find);

    const removeBox = function() {
        const box = document.getElementById('tooltip');
        if(box) box.remove();
    };

    const fillSelector = function() {
        const menudiv = document.getElementById('left_menu'); 
        menudiv.innerHTML =
 `<div id="matrixmenu" class="menubox">
    <div class="heading">Matrix</div>
      <ul>
        <li>select CSV/XML file(s): <input type="file" id="file" name="file" accept=".xml,.csv" multiple/></li>
      </ul>
</div>
<div style="display: none" id="mssmenu" class="menubox">
    <div class="heading">Texts</div>
    <ul class="ms">
    </ul>
</div>
<div style="display: none" id="treemenu" class="menubox">
    <div class="heading">Trees</div>
        <ul class="tree">
            <li>select NeXML file: <input type="file" id="treefile" name="treefile" accept=".xml"/></li>
        </ul>
    </div>
`;
        /*
    var mss = Array.from(_texts.keys());
    mss.sort();
    var msshtml = '';
    for(const ms of mss)
        msshtml += `<li data-name="${ms}">${_texts.get(ms).desc}</li>`;
    menudiv.querySelector('.ms').innerHTML = msshtml;
*/
        const menu = document.getElementById('menu');
        menu.addEventListener('mouseover', events.menuMouseover);
        menu.addEventListener('mouseout', events.menuMouseout);
        menu.addEventListener('click',events.menuClick);
        // }
        menu.querySelector('#file').addEventListener('change',fileSelect.bind(null,csvOrXml),false);
        menu.querySelector('#treefile').addEventListener('change',fileSelect.bind(null,treeFileLoad),false);
    };

    const newBox = {
        matrix: function() {
            _state.matrix = new MatrixBox();
            _state.matrix.init();
            _state.matrix.show();
            document.getElementById('matrixmenu').style.display = 'none';
            drawTrees();
            multi.rehighlight();
            return _state.matrix;
        },

        text: function(name,/*map*/) {
            const newEd = new EdBox(name);
            //const newEd = new EdBox(name,map);
            _state.textboxes.push(newEd);
            newEd.init();
            newEd.show();
            //underlineVariants();
            drawTrees();
            multi.rehighlight();
            if(!document.querySelector('.highlit'))
                events.textClick({target: newEd.boxdiv.querySelector('.lemma:not(.invisible)')});
            return newEd;
        },

        tree: function(stemmaid,id) {
            const newTree = new TreeBox(stemmaid,id);
            _state.trees.push(newTree);
            newTree.init();
            newTree.show();
            drawTrees();
            multi.rehighlight();
            return newTree;
        },
    };

    const drawTrees = function() {
        for(const tree of _state.trees) {
        //if(!tree.closed)
            tree.draw();
        }
    };

    const clearSelection = function() {
        const sel = window.getSelection ? window.getSelection() : document.selection;
        if (sel) {
            if (sel.removeAllRanges) {
                sel.removeAllRanges();
            } else if (sel.empty) {
                sel.empty();
            }
        }
    };

    const touchUpNode = function(node) {
        const walker = document.createTreeWalker(node,NodeFilter.SHOW_TEXT);
        while(walker.nextNode()) {
            const txt = touchUpText(walker.currentNode.nodeValue);
            walker.currentNode.nodeValue = txt;
        }
        // hyphenate(node,'sa');
    };

    const touchUpText = function(str) {
        return Hypher.hyphenateText(
            str
                .replace(/ \|/g,'\u00a0|')
                .replace(/\| (?=\d)/g,'|\u00a0')
                .replace(/\|\|/g,'॥')
                .replace(/\|/g,'।')
        );
    };

    /*** Multi-window updating functions ***/

    const multi = {
        getAllWindows: function() {
            return window.mainWindow ?
                window.mainWindow.comboView.getWindows() :
                _state.windows;
        },
    
        forEachWindow: function(fn) {
            const windows = multi.getAllWindows();
            for(const win of windows) {
                if(win.closed) continue;
                if(fn(win) === false) break;
            }
        },

        highlightLemma: function(n,h) {
            var hide_invisibles = h ? true : false;
            multi.forEachWindow(win => {
                const to_highlight = win.comboView.getViewdiv().querySelectorAll('[data-n="'+n+'"]');
                for(const th of to_highlight) {
                    if(!hide_invisibles || !th.classList.contains('invisible'))
                        th.classList.add('highlit');
                }
                const trans_highlight = win.comboView.getViewdiv().querySelectorAll('[data-n-trans="'+n+'"]');
                for(const trh of trans_highlight) {
                    trh.classList.add('translit');
                }
            });
        },
        
        highlightRange: function(nums) {
            if(!nums || nums.size === 0) return;
            multi.unHighlightAll();
        
            const [low,high] = Find.lowhigh(nums);
            if(high !== undefined) {
                for(let n=low;n<=high;n++) multi.highlightLemma(n,true);
            }
            else
                multi.highlightLemma(low);
            multi.repopulateTrees(low,high);
            for(const box of _state.viewdiv.querySelectorAll('.text-box'))
                if(!box.querySelector('.highlit'))
                    box.querySelector('[data-n="'+low+'"]').classList.add('highlit');      
            view.xScroll(low);
        },

        highlightAll: function() {
            if(Check.anyhighlit()) multi.unHighlightAll();
            else multi.highlightRange(new Set([0,Find.maxlemma()]));
        },

        unHighlightAll: function() {
            multi.forEachWindow(win => {
                const unlight = win.comboView.getViewdiv().querySelectorAll('.highlit');
                for(const ul of unlight) {
                    ul.classList.remove('highlit');
                }
                const untranslight = win.comboView.getViewdiv().querySelectorAll('.translit');
                for(const utl of untranslight) {
                    utl.classList.remove('translit');
                }
                multi.unCelllightAll();
            });
        },
        unCelllightAll: function() {
            multi.forEachWindow(win => {
                const uncelllight = win.comboView.getViewdiv().querySelectorAll('.highlitcell');
                for(const ucl of uncelllight) {
                    ucl.classList.remove('highlitcell');
                }
            });
        },

        rehighlight: function() {
            var highlit;
            multi.forEachWindow(win => {
                highlit = win.document.querySelectorAll('.highlit');
                if(highlit.length > 0) return false;
            });
            if(highlit.length > 0) {
                var nums = new Set();
                for(const lit of highlit)
                    nums.add(lit.dataset.n);
                multi.highlightRange(nums);
            }
        },

        clearTrees: function() {
            multi.forEachWindow(win => {
                const trees = win.comboView.getTrees();
                for(const tree of trees) {
                    tree.clearlemmata();
                }
            });
        },

        repopulateTrees: function(n,m) {
            multi.forEachWindow(win => {
                const trees = win.comboView.getTrees();
                for(const tree of trees) {
                //if(!tree.closed) {
                    tree.populate(n,m);
                    tree.colourizeVariants(n,m);
                    /*if(tree.script !== 0 )*/ tree.updatescript();
                //}
                }
                // ugly hack
                /*    for(const el of document.getElementsByClassName('tree-lemma'))
            if(el.textContent === '')
                el.innerHTML = '&nbsp;&nbsp;&nbsp;';
    */
            });
        },
        highlightTreeLemma: function(id) {
            multi.forEachWindow(win => {
                const trees = win.comboView.getTrees();
                for(const tree of trees) {
                //if(tree.closed) continue;
                    const targ = tree.boxdiv.querySelector('.tree-lemma[data-id="'+id+'"]');
                    const lemmata = tree.boxdiv.querySelectorAll('.tree-lemma');
                    for(const lemma of lemmata) {
                        if(targ.dataset.nodes && lemma.dataset.nodes === targ.dataset.nodes)
                            lemma.classList.add('highlit');
                        else if(!targ.dataset.nodes && lemma.dataset.id === targ.dataset.id)
                            lemma.classList.add('highlit');
                    }
                    //tree.clearsvg();
                    //tree.drawlines(targ.dataset.nodes,targ.style.color);
                    tree.clearLabels();
                }
            });
        },
        unhighlightTrees: function() {
            multi.forEachWindow(win => {
                const highlit = win.comboView.getViewdiv().querySelectorAll('.tree-lemma.highlit');
                for(const el of highlit) el.classList.remove('highlit');
                const trees = win.comboView.getTrees();
                for(const tree of trees) {
                //tree.clearsvg();
                //tree.drawlines();
                    tree.clearLabels();
                }
            });
        },

    };

    /*** Event listeners ***/

    const fileSelect = function(func,e) {
        const f = e.target.files[0];
        const fs = [...e.target.files].slice(1);
        const reader = new FileReader();
        reader.onload = func.bind(null,f,fs);
        reader.readAsText(f);
    };
    
    const csvOrXml = function(f,fs,e) {
        _state.filename = f.name;
        document.title = document.title + `: ${_state.filename}`;
        const ext = _state.filename.split('.').pop();
        if(ext === 'csv') csvLoad(f,fs,e);
        else matrixLoad(f,fs,e);
    };

    const treeFileLoad = function(f,fs,e) {
        const treestr = e.target.result;
        const parser = new DOMParser();
        const nexml = parser.parseFromString(treestr,'text/xml');
        const xenoData = _state.xml.querySelector('teiHeader > xenoData') || (function() {
            const header = _state.xml.querySelector('teiHeader') || (function() {
                const h = _state.xml.createElementNS(_const.teins,'teiHeader');
                _state.xml.documentElement.appendChild(h);
                return h;})();
            const newel = _state.xml.createElementNS(_const.teins,'xenoData');
            header.appendChild(newel);
            return newel;
        })();
        const stemmael = _state.xml.createElementNS(_const.teins,'stemma');
        stemmael.setAttribute('format','nexml');
        stemmael.id = 'stemma' + [...xenoData.querySelectorAll('stemma')].length;
        stemmael.appendChild(nexml.firstChild.cloneNode(true));
        xenoData.appendChild(stemmael);

        // labels are correct with new version of SplitsTree5
        /*
        const otunodes = nexml.querySelectorAll('node[otu]');
        for(const otunode of otunodes) {
            const label = otunode.getAttribute('label');
            const otu = otunode.getAttribute('otu');
            if(label !== otu) otunode.setAttribute('label',otu);
        }
        */
        treeXMLLoad(nexml,stemmael.id);
    };

    const treeXMLLoad = function(nexml,stemmaid,show=true) {
        const trees = nexml.querySelectorAll('tree');
        const treemenu = document.querySelector('#treemenu ul');
        for(const tree of trees) {
            const id = tree.id;
            const xclone = nexml.cloneNode(true);
            for(const tclone of xclone.querySelectorAll('tree')) {
                if(tclone.id !== id)
                    tclone.parentNode.removeChild(tclone);
            }
            const label = tree.getAttribute('label') || 'New Tree ' + _state.treelist.size;
            _state.treelist.set(`#${stemmaid} #${id}`,xclone);
            const li = document.createElement('li');
            li.dataset.name = label;
            li.dataset.treeid = id;
            li.dataset.stemmaid = stemmaid;
            li.appendChild(document.createTextNode(label));
            treemenu.insertBefore(li,treemenu.lastElementChild);

            if(show) newBox.tree(stemmaid,id);
        }
    };

    const csvLoad = function(f,fs,e) {
        const csvarr = CSV.parse(e.target.result,{delimiter: ','});
        _state.xml = document.implementation.createDocument(_const.teins,'',null);
        const teicorpus = _state.xml.createElementNS(_const.teins,'teiCorpus');
        const teiheader = _state.xml.createElementNS(_const.teins,'teiHeader');
        teicorpus.appendChild(teiheader);
        _state.xml.appendChild(teicorpus);

        for(const c of csvarr) {
            const name = c[0];
            const arr = c.slice(1);
            const tei = _state.xml.createElementNS(_const.teins,'TEI');
            tei.setAttribute('n',name);
            const text = _state.xml.createElementNS(_const.teins,'text');
            for(let n=0;n<arr.length;n++) {
                const word = arr[n];
                const newEl = _state.xml.createElementNS(_const.teins,'w');
                if(word)
                    newEl.appendChild(document.createTextNode(word));
                newEl.setAttribute('n',n);
                text.appendChild(newEl);
            }
            tei.appendChild(text);
            teicorpus.appendChild(tei);
        }
        _state.xml.normalize();

        _state.maxlemma = Find.maxlemma();
        
        //if(fs.length > 0) csvLoadAdditional(fs);

        menuPopulate();
    };

    const matrixLoad = function(f,fs,e) {
        const xParser = new DOMParser();
        _state.xml = xParser.parseFromString(e.target.result,'text/xml');
        
        const trees = _state.xml.querySelectorAll('teiHeader xenoData stemma nexml');
        for(const tree of trees) {
            const nexml = document.implementation.createDocument('http://www.nexml.org/2009','',null);
            nexml.appendChild(tree.cloneNode(true));
            treeXMLLoad(nexml,tree.closest('stemma').id,false);
        }

        _state.maxlemma = Find.maxlemma();
        
        if(fs.length > 0) matrixLoadAdditional(fs);

        else menuPopulate();

    };

    const matrixLoadAdditional = function(fs) {
        const f = fs[0];
        const fss = fs.slice(1);
        const setDiff = function(setA,setB) {
            const ret = new Set(setA);
            for(const el of setB) ret.delete(el);
            return ret;
        };

        const go = function(add,e) {
            const xParser = new DOMParser();
            const newxml = xParser.parseFromString(e.target.result,'text/xml');
            
            const oldteis = new Map();
            for(const tei of Find.teis()) {
                const siglum = tei.getAttribute('n');
                oldteis.set(siglum,tei);
            }
            const oldsigla = new Set(oldteis.keys());

            const newteis = new Map();
            for(const tei of newxml.querySelectorAll('TEI')) {
                const siglum = tei.getAttribute('n');
                newteis.set(siglum,tei);
            }
            const newsigla = new Set(newteis.keys());

            const tofill = setDiff(oldsigla,newsigla);
            const toadd  = setDiff(newsigla,oldsigla);

            
            for(const el of toadd) make.tei(el);

            const addlemma = [...newxml.querySelector('text').querySelectorAll('w[n]')].length;
            const lastlemma = _state.maxlemma + addlemma;
            for(const [key,val] of newteis) {
                if(!oldteis.has(key)) {
                    const newtei = _state.xml.createElementNS(_const.teins,'TEI');
                    newtei.setAttribute('n',key);
                    const newtext = _state.xml.createElementNS(_const.teins,'text');
                    make.emptywords(newtext,lastlemma,0);
                    newtei.appendChild(newtext);
                    _state.xml.documentElement.appendChild(newtei);
                }
                else {
                    const oldtext = oldteis.get(key).querySelector('text');
                    const newtext = val.querySelector('text');
                    const newwords = newtext.querySelectorAll('w[n]');
                    var cur_n = _state.maxlemma + 1;
                    for(const word of newwords) {
                        word.setAttribute('n',cur_n);
                        oldtext.appendChild(word);
                        cur_n = cur_n + 1;
                    }
                }
            }

            for(const el of tofill) {
                //const oldtext = oldteis.get(el).querySelector('text');
                const text = oldteis.get(el).querySelector('text');
                make.emptywords(text,lastlemma,_state.maxlemma + 1);
            }

            _state.maxlemma = Find.maxlemma(); // can probably change this
            if(add.length > 0) matrixLoadAdditional(add);
            else menuPopulate();
        };

        const reader = new FileReader();
        reader.onload = go.bind(null,fss);
        reader.readAsText(f);
        
    };

    const mssMenuPopulate = function() {
        var msshtml = '';
        const mss = [...Find.teis()].map(el => el.getAttribute('n'));
        for(const ms of mss) {
        //    msshtml += `<li data-name="${ms}">${_texts.get(ms).desc}</li>`;
            msshtml += `<li data-name="${ms}">${ms}</li>`;
        }
        document.getElementById('menu').querySelector('.ms').innerHTML = msshtml;
    };

    const menuPopulate = function() {
        mssMenuPopulate();
        const expbox = new menuBox('Export');
        expbox.populate([
            {text: 'TEI corpus', func: exp.showOptions.bind(null,exp.xml,exp.options)},
            {text: 'TEI apparatus', func: exp.showOptions.bind(null,exp.xml,exp.appOptions)},
            {text: 'CSV', func: exp.showOptions.bind(null,exp.csv,exp.options)},
            {text: 'NEXUS', func: exp.showOptions.bind(null,exp.nexus,exp.options)}
        ]);
        const editbox = new menuBox('Edit');
        editbox.populate([
            {text: 'Undo', greyout: Check.undo, func: edit.undo},
            {text: 'Redo', greyout: Check.redo, func: edit.redo},
            {text: 'Select all',
                alt: 'Deselect all',
                toggle: Check.anyhighlit,
                func: multi.highlightAll
            },
            {text: '(Re)normalize columns',
                greyout: Check.anyhighlit,
                func: edit.startRenormalize.bind(null,false)
            },
            {text: 'Delete column',
                greyout: Check.anyhighlit,
                func: edit.startRemoveCol.bind(null,false)
            },
            {text: 'Delete empty columns',
                func: edit.startRemoveEmpty
            },
            {text: 'Merge columns',
                greyout: Check.manyhighlit,
                func: edit.startMerge.bind(null,false)
            },
            {text: 'Group columns',
                alt: 'Ungroup columns',
                greyout: Check.oneGrouped,
                toggle: Check.grouped,
                func: edit.startGroup.bind(null,false),
            },
            {text: 'Group words',
                func: edit.doGroupWords
            },
            {text: 'Edit cell',
                greyout: Check.highlitcell,
                func: edit.startEditCell.bind(null,false),
            },
            {text: 'Insert row',
                func: edit.startNewRow,
            },
            {text: 'Insert column',
                greyout: Check.anyhighlit,
                func: edit.startInsertCol
            },
            {text: 'Mark insignificant',
                checkbox: Check.checkbox.bind(null,'insignificant',false),
                greyout: Check.anyhighlit,
                func: edit.startMarkAs.bind(null,'insignificant',false)
            },
            {text: 'Mark binary',
                checkbox: Check.checkbox.bind(null,'binary',false),
                greyout: Check.anyhighlit,
                func: edit.startMarkAs.bind(null,'binary',false)
            },
        ]);

        const viewbox = new menuBox('View');
        viewbox.populate([
            {text: 'Header',
                checkbox: Check.headerView,
                func: view.toggleHeader,
            },
            {text: 'Normalized',
                checkbox: Check.normalizedView,
                greyout: Check.anyNormalized,
                func: view.toggleNormalize,
            },
        ]);

        const left_menu = document.getElementById('left_menu');
        left_menu.appendChild(viewbox.box);
        left_menu.appendChild(editbox.box);
        left_menu.appendChild(expbox.box);
   
        const views = document.getElementById('views');
        views.style.justifyContent = 'flex-start';
        views.removeChild(views.querySelector('#splash'));

        newBox.matrix();
        const matrixmenu = document.getElementById('matrixmenu');
        matrixmenu.addEventListener('click',newBox.matrix);
        matrixmenu.removeChild(matrixmenu.querySelector('ul'));
        document.getElementById('mssmenu').style.display = 'block';
        document.getElementById('treemenu').style.display = 'block';

    };

    const exp = {
        write: async function(file,handle) {
            const writer = await handle.createWritable();
            writer.write(file);
            writer.close();
        },

        xml: async function(doc) {
            const str = new XMLSerializer().serializeToString(
                //  XSLTransformElement(_state.xml,xslt_proc)
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
            //const str = new XMLSerializer().serializeToString(XSLTransformElement(doc,xslt_proc));
            //const str = new XMLSerializer().serializeToString(xslt_proc.transformToDocument(_state.xml));
            //const str = xslt_proc.transformToDocument(_state.xml).documentElement.textContent;
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
            const doc = _state.xml.cloneNode(true);

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
            const teiheader = newdoc.createElementNS(_const.teins,'teiHeader');
            const filedesc = newdoc.createElementNS(_const.teins,'fileDesc');
            const sourcedesc = newdoc.createElementNS(_const.teins,'sourceDesc');
            const listwit = newdoc.createElementNS(_const.teins,'listWit');
            for(const text of Find.teis(doc)) {
                const n = text.getAttribute('n');
                const wit = newdoc.createElementNS(_const.teins,'witness');
                wit.setAttribute('xml:id',n);
                const idno = newdoc.createElementNS(_const.teins,'idno');
                idno.setAttribute('type','siglum');
                idno.append(n);
                wit.appendChild(idno);
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

            const newdoc = document.implementation.createDocument(_const.teins,'',null);
            newdoc.appendChild(basetext);
            const teiheader = exp.makeHeader(newdoc,doc);
            basetext.insertBefore(teiheader,basetext.firstChild);
            const words = Find.words(false,basetext);
            for(const word of words) {
                const dataN = word.getAttribute('n');
                const lemma = normlem ? 
                    (word.getAttribute('lemma') || word.textContent) :
                    word.textContent;
                const posapp = [];
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
                    const app = newdoc.createElementNS(_const.teins,'app');
                    app.setAttribute('n',dataN);
                    const lem = newdoc.createElementNS(_const.teins,'lem');
                    const poswits = posapp.map(s => `#${s}`).join(' ');
                    if(poswits !== '') lem.setAttribute('wit',poswits);
                    while(word.firstChild)
                        lem.appendChild(word.firstChild);
                    app.appendChild(lem);
                    for(const [str,wits] of negapp) {
                        const rdg = newdoc.createElementNS(_const.teins,'rdg');
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
    };
    
    /*
    const fullTreeMouseover = function(e) {
        const targ = e.target.classList.contains('littletree') ?
            e.target :
            e.target.closest('.littletree');
        if(targ) {
            const littlelines = document.getElementById('full-tree').querySelectorAll('line.littletree');
            for(const line of littlelines) {
                line.style.stroke = 'rgb(179,18,125)';
            }
            const littletext = document.getElementById('full-tree').querySelectorAll('text.littletree');
            for(const text of littletext) {
                text.style.fill = 'rgb(179,18,125)';
            }
        }
    };
    
    const fullTreeMouseout = function(e) {
        const targ = e.target.classList.contains('littletree') ?
            e.target :
            e.target.closest('.littletree');
        if(targ) {
            const littlelines = document.getElementById('full-tree').querySelectorAll('line.littletree');
            for(const line of littlelines) {
                line.style.stroke = '#80a0ff';
            }
            const littletext = document.getElementById('full-tree').querySelectorAll('text.littletree');
            for(const text of littletext) {
                text.style.fill = 'black';
            }
        }
    };
    */
    /*
const fullTreeClick = function(e) {
    const targ = e.target.classList.contains('littletree') ?
        e.target :
        e.target.closest('.littletree');
    if(targ) {
        document.getElementById('full-tree').style.display = 'none';
        comboView.maininit();
        newBox.tree(document.querySelector('.tree li').dataset.name);
    }
}
*/
    const events = {
        selectAll(el) {
            const range = document.createRange();
            range.selectNodeContents(el);
            const sel = window.getSelection();
            sel.removeAllRanges();
            sel.addRange(range);
        },

        deselect() {
            const sel = window.getSelection();
            if(sel.removeAllRanges) sel.removeAllRanges();
            else if(sel.empty) sel.empty();
        },

        treeMouseover(e) {
            const targ = e.target.classList.contains('tree-lemma') ?
                e.target :
                e.target.closest('.tree-lemma');

            if(targ) {
                multi.highlightTreeLemma(targ.dataset.id);
                targ.addEventListener('mouseout',multi.unhighlightTrees);
            }
        
            const title = e.target.dataset.reconstructed;
            if(!title) return;

            const box = document.createElement('div');
            box.id = 'tooltip';
            box.style.top = e.pageY + 'px';//(e.clientY + 10) + 'px';
            box.style.left = e.pageX + 'px';//e.clientX + 'px';
            box.style.opacity = 0;
            box.style.transition = 'opacity 0.2s ease-in';
            _state.viewdiv.parentElement.appendChild(box);

            const textbox = document.createElement('div');
            textbox.appendChild(document.createTextNode(title));
            this.script !== 0 ? // this is bound to the TreeBox 
                box.appendChild(changeScript(textbox,_const.scripts[this.script])) :
                box.appendChild(textbox);

            if(e.target.classList.contains('reconstructed')) {
                const treelemma = e.target.parentNode.querySelector('span.tree-lemma');
                if(treelemma && treelemma.dataset.hasOwnProperty('emended')) {
                    const emendbox = document.createElement('div');
                    emendbox.classList.add('emphasis');
                    emendbox.appendChild(document.createTextNode(treelemma.textContent));
                    this.script !== 0 ?
                        box.prepend(changeScript(emendbox,_const.scripts[this.script])) :
                        box.prepend(emendbox);
                }
            }

            window.getComputedStyle(box).opacity;
            box.style.opacity = 1;
        
            e.target.addEventListener('mouseout', removeBox);
        },

        treeClick(e) {
            if(e.target.classList.contains('witness'))
                newBox.text(e.target.dataset.key);
            //newBox.text(e.target.dataset.key,_texts);
            else if(e.target.classList.contains('reconstructed'))
                newBox.text(e.target.dataset.label);
            //newBox.text(e.target.dataset.label,_texts);
            else if(e.target.classList.contains('internal'))
                edit.startReconstruction(e);
        
            removeBox();
        },

        keyDown(e) {
            if(!_state.editing && e.key.substring(0,5) === 'Arrow') events.cycleVariant(e);
            else if(e.ctrlKey || e.metaKey) {
                if(e.key === 'Z')
                    edit.redo();
                else if(e.key === 'z')
                    edit.undo();
            }
            else if(!_state.editing &&
                    _state.matrix && !_state.matrix.closed &
                    e.key === 'Enter') {
                const td = Find.highlitcell();
                if(td) {
                    e.preventDefault();
                    edit.startEditCell(td);
                }
            }
        },

        cycleVariant(e) {
            const highlitcell = Find.highlitcell() || 
                _state.viewdiv.querySelector('td.highlit') ||
                _state.viewdiv.querySelector('td[data-n="0"]');

            switch (e.key) {

            case 'ArrowRight': {
                if(!_state.matrix.closed && highlitcell) {
                    const next = highlitcell.nextElementSibling;
                    if(next) events.textClick({target: next});
                } 
                else {
                    const highlit = _state.viewdiv.querySelector('.highlit');
                    const cur = highlit ? highlit.dataset.n : 0;
                    let next = parseInt(cur)+1;
                    while(next <= _state.maxlemma) {
                        const nextlemmata = _state.viewdiv.querySelectorAll('[data-n="'+next+'"]');
                        for(const nextlemma of nextlemmata) {
                            if(nextlemma && !nextlemma.classList.contains('invisible')) {
                                events.textClick({target: nextlemma});
                                return;
                            }
                        }
                        next++;
                    }
                }
                break;
            }
            case 'ArrowLeft': {
                if(!_state.matrix.closed && highlitcell) {
                    const prev = highlitcell.previousElementSibling;
                    if(prev) events.textClick({target: prev});
                }
                else {
                    const highlit = _state.viewdiv.querySelector('.highlit');
                    const cur = highlit ? highlit.dataset.n : 0;
                    let prev = parseInt(cur) -1;
                    while(prev >= 0) {
                        const prevlemmata = _state.viewdiv.querySelectorAll('[data-n="'+prev+'"]');
                        for(const prevlemma of prevlemmata) {
                            if(prevlemma && !prevlemma.classList.contains('invisible')) {
                                events.textClick({target: prevlemma});
                                return;
                            }
                        }
                        prev--;                        if(highlitcell) {
                            events.textClick({target: highlitcell.previousElementSibling});
                            return;
                        }
                    }
                }
                break;
            }
            case 'ArrowUp': {
                if(_state.matrix.closed || !highlitcell) return;

                const tr = highlitcell.closest('tr'); 
                const prevtr = tr.previousElementSibling;
                if(!prevtr || !prevtr.dataset.n) return;
                const newtd = prevtr.querySelector(`td[data-n="${highlitcell.dataset.n}"]`);
                events.textClick({target: newtd});
                
                break;
            }
            case 'ArrowDown': {
                if(_state.matrix.closed || !highlitcell) return;
                const tr = highlitcell.closest('tr'); 
                const nexttr = tr.nextElementSibling;
                if(!nexttr || nexttr.tagName !== 'TR') return;
                const newtd = nexttr.querySelector(`td[data-n="${highlitcell.dataset.n}"]`);
                events.textClick({target: newtd});
            }
            } // end switch
        },

        textClick(e,skipRight = false) {
            if(e.target.closest('tr.header')) {
                events.matrixHeaderClick(e);
                return;
            }
            const targ = e.target.classList.contains('lemma') ? 
                e.target :
                e.target.closest('.lemma');

            if(targ) {
                if(!skipRight && e.ctrlKey) {
                    events.rightClick(e);
                    return;
                }
                const n = targ.dataset.n;
                const matrixrow = Find.highlitrow();
                multi.unHighlightAll();
                multi.highlightLemma(n);
                multi.repopulateTrees(n);
                view.xScroll(n,matrixrow);
                if(targ.tagName === 'TD')
                    targ.classList.add('highlitcell');
                else {
                    const textbox = targ.closest('.text-box');
                    if(textbox) {
                        const textid = textbox.dataset.id;
                        const td = Find.tr(textid).querySelector(`td[data-n="${n}"]`);
                        td.classList.add('highlitcell');
                    }
                }

            }
        },

        textMouseup() {
            const nums = Find.selection();
            multi.highlightRange(nums);
            clearSelection();
        },

        /*const lemmaMouseover = function(e) {
        
        const title = e.target.dataset.title;
        if(!title) return;

        const box = document.createElement('div');
        box.id = 'tooltip';
        box.style.top = e.pageY + 'px';//(e.clientY + 10) + 'px';
        box.style.left = e.pageX + 'px';//e.clientX + 'px';
        box.style.opacity = 0;
        box.style.transition = 'opacity 0.2s ease-in';
        _state.viewdiv.parentElement.appendChild(box);

        const textbox = document.createElement('div');
        textbox.appendChild(document.createTextNode(title));
        box.appendChild(textbox);
        window.getComputedStyle(box).opacity;
        box.style.opacity = 1;
        
        e.target.addEventListener('mouseout', removeBox);
    };
    */

        menuMouseover(e) {
            const targ = e.target.classList.contains('menubox') ?
                e.target :
                e.target.closest('.menubox');
            if(targ) {
                const ul = targ.querySelector('ul');
                if(ul) ul.style.display = 'block';
                targ.classList.add('open');
            }
        },
        menuMouseout(e) {
            const targ = e.target.classList.contains('menubox') ?
                e.target :
                e.target.closest('.menubox');
            if(targ) {
                const ul = targ.querySelector('ul');
                if(ul) ul.style.display = 'none';
                targ.classList.remove('open');
            }
        },
        menuClick(e) {
            if(!e.target.parentElement) return;
            /*
        if(e.target.parentElement.className === 'ed') {
            events.menuMouseout(e);
            newBox.text(e.target.dataset.name,_editions);
        }
    */
            if(e.target.parentElement.className === 'ms') {
                events.menuMouseout(e);
                newBox.text(e.target.dataset.name);
            //newBox.text(e.target.dataset.name,_texts);
            }
            if(e.target.parentElement.className === 'tree') {
                events.menuMouseout(e);
                if(e.target.closest('li[data-name]'))
                    newBox.tree(e.target.dataset.stemmaid,e.target.dataset.treeid);
            }
        },

        thDragStart(e) {
            e.dataTransfer.setData('text/plain',e.target.textContent);
            //    _state.dragged.parentNode.classList.add('dragging');
            edit.startMoveRow(e.target.parentNode,e);
        },
        trDragEnter(e) {
            const tr = e.target.nodeType === 1 ? 
                e.target.closest('tr') :
                e.target.parentElement.closest('tr');
            if(tr)
                tr.classList.add('dragenter');
        },
        trDragLeave(e) {
            const tr = e.target.nodeType === 1 ?
                e.target.closest('tr') :
                e.target.parentElement.closest('tr'); 
            if(tr)
                tr.classList.remove('dragenter');
        },

        trDragDrop(e) {
            e.preventDefault();
            /*    _state.dragged.parentNode.classList.remove('dragging');
        const tr = e.target.nodeType === 1 ?
                e.target.closest('tr') :
                e.target.parentElement.closest('tr');
        if(tr) {
            tr.classList.remove('dragenter');
            edit.doMoveRow(_state.dragged.parentNode,tr);
            _state.dragged = null;
        }
        */
            edit.finishMoveRow(e);
        },

        matrixMousedown(e) {
            if(e.button !== 0) return;
            if(e.ctrlKey) {events.rightClick(e); return;}
            const lemma = e.target.nodeType === 1 ?
                e.target.closest('.lemma') :
                e.target.parentElement.closest('.lemma');
            if(lemma) {
            
                if(lemma.isContentEditable) return;

                multi.unHighlightAll();
                multi.highlightLemma(lemma.dataset.n);
                const tabl = _state.matrix.boxdiv.querySelector('table');
                tabl.addEventListener('mouseover',events.matrixMouseover);
                tabl.addEventListener('mouseup',events.matrixMouseup);
            }
        },
        matrixMouseup(/*e*/) {
            const nums = Find.highlit();
            multi.highlightRange(nums);
            const tabl = _state.matrix.boxdiv.querySelector('table');
            tabl.removeEventListener('mouseover',events.matrixMouseover);
            tabl.removeEventListener('mouseup',events.matrixMouseup);
        },
        matrixMouseover(e) {
            const lemma = e.target.nodeType === 1 ?
                e.target.closest('.lemma') :
                e.target.parentElement.closest('.lemma');
            if(lemma)
                multi.highlightLemma(lemma.dataset.n);
        },
        matrixHeaderClick(e) {
            if(e.target.tagName !== 'INPUT') return;
            const type = e.target.className;
            if(type !== 'insignificant' && type !== 'binary') return;
            const num = e.target.closest('th').dataset.ref;
            const state = Find.firsttd(num).dataset[type] === 'true' ? false : true;
            const states = new Map([[num,state]]);
            //if(Find.firsttd(num).dataset[type] === 'true') e.target.checked = true;
            //else e.target.checked = false;
            //edit.startMarkAs(e.target.className,nums,e);
            edit.doMarkAs(type,states);
        },

        rightClick(e) {
            const th = e.target.nodeType === 1 ?
                e.target.closest('tr[data-n] th') :
                e.target.parentElement.closest('tr[data-n] th');
            if(th) {
                e.preventDefault();
                contextMenu.remove();
                const menu = contextMenu.create(e);
                const items = [
                    {text: 'move row',
                        func: edit.startMoveRow.bind(null,th.parentNode),
                    },
                    {
                        text: 'delete row',
                        func: edit.doDeleteRow.bind(null,th.parentNode.dataset.n),
                    }
                ];
                contextMenu.populate(menu,items);
                contextMenu.show(menu);
                return;
            }

            const td = e.target.nodeType === 1 ?
                e.target.closest('td.lemma') :
                e.target.parentElement.closest('td.lemma');
            if(td) {
                e.preventDefault();
                const nums = !td.classList.contains('highlit') ?
                    (events.textClick(e,true), new Set([td.dataset.n])) :
                    (function() {
                        const ret = Find.highlit();
                        if(ret.size === 1 && 
                       !td.classList.contains('highlitcell')) {
                            multi.unCelllightAll(); 
                            td.classList.add('highlitcell');
                        }
                        return ret;
                    })();
                const items = nums.size > 1 ? 
                    [
                        {text: 'merge columns',
                            func: edit.startMerge.bind(null,nums)
                        },
                        {text: 'ungroup columns',
                            alt: 'group columns',
                            toggle: Check.grouped,
                            func: edit.startGroup.bind(null,false)
                        },
                        {text: 'delete columns',
                            func: edit.startRemoveCol.bind(null,nums)
                        },
                        /*                {text: 'insignificant',
                     cond: Check.checkbox.bind(null,'insignificant',nums),
                     func: edit.startMarkAs.bind(null,'insignificant',nums),
                    },
                    {text: 'binary',
                     cond: Check.checkbox.bind(null,'binary',nums),
                     func: edit.startMarkAs.bind(null,'binary',nums),
                    }, */
                    ] : 
                    [
                        {text: 'edit cell',
                            func: edit.startEditCell.bind(null,td)
                        },
                        {text: 'delete column',
                            func: edit.startRemoveCol.bind(null,nums)
                        },
                        {text: 'insert column',
                            func: edit.startInsertCol
                        },
                        /*                {text: 'insignificant',
                     cond: Check.checkbox.bind(null,'insignificant',nums),
                     func: edit.startMarkAs.bind(null,'insignificant',nums),
                    },
                    {text: 'binary',
                     cond: Check.checkbox.bind(null,'binary',nums),
                     func: edit.startMarkAs.bind(null,'binary',nums),
                    }, */
                    ];
                contextMenu.remove();
                const menu = contextMenu.create(e);
                contextMenu.populate(menu,items);
                contextMenu.show(menu);
            }
        },
        
    };

    const edit = {
        undo: function() {
            const action = _state.undo.pop();
            if(action)
                action[0](...action[1]);
        },
        redo: function() {
            const action = _state.redo.pop();
            if(action)
                action[0](...action[1]);
        },
        doStack: function(entry,doing = 'do') {
            if(doing === 'undo') {
                entry[1].push('redo');
                _state.redo.push(entry);
            }
            else if(doing === 'redo') {
                entry[1].push('undo');
                _state.undo.push(entry);
            }
            else {
                entry[1].push('undo');
                _state.undo.push(entry);
                _state.redo = [];
            }
        },
    
        doMulti: function(dolist,doing) {
            const undolist = [];
            for(const item of dolist) {
                const ret = item[0](...item[1],'multido');
                undolist.unshift(ret);
            }
            edit.doStack([edit.doMulti,[undolist]],doing);
        },


        startMoveRow: function(targ,e) {
            _state.dragged = targ;
            _state.dragged.classList.add('dragging');
            if(e.type !== 'dragstart') {
                for(const tr of Find.trs())
                    tr.classList.add('moveinprogress');
                _state.matrix.boxdiv.querySelector('table').addEventListener('mousedown',edit.finishMoveRow);
            }
        },

        startMerge: function(nums,/*e*/) {
            const numss = nums === false ?
                Find.highlit() :
                nums;
            const clgroups = Find.clauses(numss,true);
            if(!clgroups) {
                edit.doMerge(numss,'do');
            }
            else if(clgroups[0] === null) {
                const args = clgroups.filter(s => s!== null)
                    .map(s => [edit.doUngroup,[s]])
                    .concat([[edit.doMerge,[numss]]]);
                edit.doMulti(args,'do');
            }
            else {
                const toremove = Find.clausesToRemove(clgroups,numss);
                if(!toremove)
                    edit.doMerge(numss,'do');
                else {
                    const args = toremove.map(s => [edit.doUngroup,[s]])
                        .concat([[edit.doMerge,[numss]]]);
                    edit.doMulti(args,'do');
                }

            }
        },

        startGroup: function(nums,/*e*/) {
            const numss = nums === false ?
                Find.highlit() :
                nums;
            if(Check.grouped())
                edit.startUngroup(numss);
            else
                edit.doGroup(numss,'do');
        },

        startUngroup: function(nums) {
            /*        const firstrow = _state.xml.querySelector('text');

        // make a list of clauses
        const cls = new Set();
        for(const num of nums) {
            const word = firstrow.querySelector('w[n="'+num+'"]');
            const cl = word.closest('cl');
            if(cl) cls.add(cl);
        }
        const args = [...cls].map(function(cl) {
            const words = cl.querySelectorAll('w');
            return [edit.doUngroup,
                    [new Set([...words].map(w => w.getAttribute('n')))]
                   ];
        }); */
            const clgroups = Find.clauses(nums);
            const args = [...clgroups].map(s => [edit.doUngroup,[s]]);
            edit.doMulti(args,'do');
        },
        
        startInsertCol: function() {
            const insertafter = Math.max([...Find.highlit()]) || _state.maxlemma;
            edit.doInsertCol(insertafter);
        },

        startRemoveCol: function(nums,/*e*/) {
            const numss = nums === false ?
                Find.highlit() :
                nums;

            const clgroups = Find.clauses(numss);
            if(!clgroups) {
                edit.doRemoveCol(numss,'do');
            }
            else {
                const toremove = Find.clausesToRemove(clgroups,numss,1);
                if(!toremove)
                    edit.doRemoveCol(numss,'do');
                else {
                    const args = toremove.map(s => [edit.doUngroup,[s]])
                        .concat([[edit.doRemoveCol,[numss]]]);
                    edit.doMulti(args,'do');
                }
            }
        },

        startRemoveEmpty: function() {
            const nums = Find.empty();
            const clgroups = Find.clauses(nums);
            if(!clgroups) {
                edit.doRemoveCol(nums,'do');
            }
            else {
                const toremove = Find.clausesToRemove(clgroups,nums,1);
                if(!toremove)
                    edit.doRemoveCol(nums,'do');
                else {
                    const args = toremove.map(s => [edit.doUngroup,[s]])
                        .concat([[edit.doRemoveCol,[nums]]]);
                    edit.doMulti(args,'do');
                }
            }
        },

        startEditCell: function(el) {
        //const cell = el || document.querySelector('.matrix td.highlitcell');
            const cell = el || Find.highlitcell();
            if(!cell) return;
            //cell.classList.add('highlitcell');
            edit.unnormalize(cell);
            cell.dataset.oldContent = cell.textContent;
        
            cell.contentEditable = 'true';
            cell.focus();
            _state.editing = true;
            events.selectAll(cell);
            cell.addEventListener('blur',edit.finishEditCell);
            cell.addEventListener('keydown',edit.cellKeyDown);
        },

        cellKeyDown: function(e) {
            switch(e.key) {
            case 'Enter':
                edit.finishEditCell(e);
                break;
            case 'Escape':
                edit.finishEditCell(e,true);
                break;
            case 'ArrowRight': {
                const pos = Find.cursorPos(e.target);
                if(pos[0] === pos[1]) {
                    e.preventDefault();
                    edit.finishEditCell(e);
                    events.cycleVariant({key: 'ArrowRight'});
                    edit.startEditCell(Find.highlitcell()); 
                }
                break;
            }
            case 'ArrowLeft': {
                const pos = Find.cursorPos(e.target);
                if(pos[0] === 0) {
                    e.preventDefault();
                    edit.finishEditCell(e);
                    events.cycleVariant({key: 'ArrowLeft'});
                    edit.startEditCell(Find.highlitcell()); 
                }
                break;
            }
            
            }
        },
     
        startMarkAs: function(type,nums,e) {
            const targ = e.target.tagName === 'INPUT' ?
                e.target :
                e.target.querySelector('input');
            const numss = nums === false ?
                Find.highlit() :
                nums;
            const states = new Map([...numss].map(num => [num,!targ.checked]));
            edit.doMarkAs(type,states);
            /*        const cells = [...nums].reduce((acc,num) =>
            acc.concat([...document.querySelectorAll('.matrix table [data-n="'+num+'"]')]),
            []);
        if(targ.checked === true)
            for(const cell of cells)
                delete cell.dataset.insignificant;
        else
            for(const cell of cells)
                cell.dataset.insignificant = 'true';
        _state.undo.push([edit.unmarkSignificance,[oldstates,true]]); */
        },
    
        startNewRow: function() {
            const tr = make.row('new row');
            const th = tr.querySelector('th');
            th.contentEditable = true;
            th.addEventListener('blur',edit.finishNewRow);
            th.addEventListener('keydown',edit.thKeyDown);

            _state.matrix.boxdiv.querySelector('tbody').appendChild(tr);
            th.scrollIntoView();
            th.focus();
            document.execCommand('selectAll',false,null);
            _state.editing = true;
        },

        startRenameRow: function(/*n*/) {
        // TODO
        },

        startReconstruction: function(e) {
            const key = e.target.dataset.key;
            const tree = e.target.closest('.tree-box').myTree;
            const treename = tree.desc;
            const blackout = document.createElement('div');
            blackout.id = 'blackout';
            const frag = document.createRange().createContextualFragment(
                `<div id="reconstructionoptions" class="popup">
    <form id="reconstructionform">
      <div>
        <label for="reconstructed_node_name">Label for node:</label>
        <input type="text" id="reconstructed_node_name" name="reconstructed_node_name" placeholder="${treename}-${key}" size="15">
      </div>
      <div style="width: 100%; display: flex; justify-content: center;padding-top: 1em;">
        <button type="submit">Add to matrix</button>
      </div>
    </form>
</div>`);
            blackout.appendChild(frag);
            document.body.appendChild(blackout);
            const submitfunction = function(e) {
                e.preventDefault();
                const input = blackout.querySelector('input');
                const label = input.value ? input.value : input.placeholder;
                edit.doReconstruction(tree,key,label);
                document.body.removeChild(blackout);
            };
            const submit = blackout.querySelector('button');
            submit.addEventListener('click',submitfunction);
            blackout.addEventListener('click',exp.blackoutClick);
        },

        thKeyDown: function(e) {
            if(e.key === 'Enter')
                edit.finishNewRow(e);
        },

        finishNewRow: function(e) {
            const th = e.target;
            const label = th.textContent;
            th.closest('tr').dataset.n = label;
        
            const tei = make.tei(label);

            _state.xml.documentElement.appendChild(tei);
        
            _state.editing = false;
            th.contentEditable = false;
            th.removeEventListener('blur',edit.finishNewRow);
            th.removeEventListener('keydown',edit.thKeyDown);
            // view.updateAllHeaders(); // new row is empty
            edit.doStack([edit.doDeleteRow,[label]],'do');
        },

        finishEditCell: function(e,cancel = false) {
            const cell = e.target;
            //cell.classList.remove('highlitcell');
            _state.editing = false;
            cell.contentEditable = 'false';
            cell.removeEventListener('blur',edit.finishEditCell);
            cell.removeEventListener('keydown',edit.cellKeyDown);
            cell.blur();
            events.deselect();
            const content = cell.textContent;
            
            if(content === '') {
                const br = cell.querySelector('br');
                if(br) br.remove();
            }

            if(cancel || content === cell.dataset.oldContent) return;
        
            if(!cell.hasOwnProperty('IAST'))
                cell.IAST = cell.cloneNode(true);
            cell.IAST.textContent = content;

            const cellnum = parseInt(cell.dataset.n);
            const tr = cell.closest('tr');
            const rownum = tr.dataset.n;
            edit.xmlChangeCell(cellnum,rownum,content);

            /*
            const row = cell.closest('tr');
            //const table = row.parentNode;
            const trs = [...Find.trs()];
            const rownum = trs.indexOf(row);
            edit.xmlChangeCell(cellnum,rownum,content);
            */
            if(tr.dataset.hasOwnProperty('treename') && !cell.dataset.hasOwnProperty('emended')) {
                const emendaction = edit.doEmend(cellnum,rownum,'multido');
                const dolist = [];
                dolist.push([edit.doChangeCell,[cellnum,rownum,cell.dataset.oldContent]]);
                dolist.push(emendaction);
                edit.doStack([edit.doMulti,[dolist]],'do');
            }
            else
                edit.doStack([edit.doChangeCell,[cellnum,rownum,cell.dataset.oldContent]],'do');
            delete cell.dataset.oldContent;

            //view.renormalize(cellnum-1,cellnum+1,rownum);
            edit.refresh();
            //view.updateHeaders([cellnum]);
            view.updateAllHeaders(true);
            cell.classList.add('highlitcell');
        },

        finishMoveRow: function(e) {
            const tr = e.target.nodeType === 1 ?
                e.target.closest('tr') :
                e.target.parentElement.closest('tr');
            if(tr)
                edit.doMoveRow(_state.dragged,tr,'do');
            if(e.type !== 'drop') {
                for(const tr of Find.trs())
                    tr.classList.remove('moveinprogress');
                _state.matrix.boxdiv.querySelector('table').removeEventListener('mousedown',edit.finishMoveRow);
            }
            else {
                for(const tr of Find.trs())
                    tr.classList.remove('dragenter');
            }
            _state.dragged.classList.remove('dragging');
            _state.dragged = null;
        },
    
        doReconstruction: function(tree,key,label) {
 
            const tr = make.row(label,'pending');
            tr.dataset.treename = tree.name;
            tr.dataset.nodename = key;
            const th = tr.querySelector('th');
            const spinner = document.createElement('div');
            spinner.id = 'spinner';
            th.prepend(spinner);

            const tei = make.tei(label);
            tei.setAttribute('type','reconstructed');
            tei.setAttribute('corresp',tree.name);
            tei.setAttribute('select',`#${key}`);
        
            const tds = [...Find.tds(false,tr)];
            const words = [...Find.words(false,tei)];
        
            _state.matrix.boxdiv.querySelector('tbody').appendChild(tr);
            th.scrollIntoView();

            const workerblob = new Blob(['('+workerFunc.toString()+')()'],{type: 'application/javascript'});
            const fitchWorker = new Worker(window.URL.createObjectURL(workerblob));
            const normalized = Check.normalizedView();
            const serialreadings = Find.serializedtexts(tree.nexml,normalized);
            const seriallevels = Find.serializedlevels(tree.levels,normalized);

            fitchWorker.postMessage({readings:serialreadings,levels:seriallevels,num:0,id:key});
            fitchWorker.onmessage = function(e) {
                const n = e.data.n;
                const reading = e.data.result;
                tds[n].textContent = reading;
                tds[n].IAST = tds[n].cloneNode(true);
                tds[n].classList.remove('pending');
                words[n].textContent = reading;
                if(n < _state.maxlemma)
                    fitchWorker.postMessage({readings:serialreadings,levels:seriallevels,num:n+1,id:key});
                else {
                    th.removeChild(spinner);
                    _state.xml.documentElement.appendChild(tei);
                    view.updateAllHeaders();
                    tree.draw();
                    const hl = Find.highlit();
                    if(hl.size > 0) multi.repopulateTrees(...Find.lowhigh(hl));

                    const mslist = document.getElementById('menu').querySelector('.ms');
                    const liel = document.createElement('li');
                    liel.setAttribute('data-name',label);
                    liel.appendChild(document.createTextNode(label));
                    mslist.appendChild(liel);
                    
                    edit.doStack([edit.doDeleteRow,[label]],'do');

                }
            };
        },

        doDeleteRow: function(label,doing = 'do') {
            const htmlrow = Find.tr(label);
            const xmlrow = Find.tei(label);
            const teis = [...Find.teis()];
            const index = teis.indexOf(xmlrow);

            htmlrow.parentNode.removeChild(htmlrow);
            xmlrow.parentNode.removeChild(xmlrow);
            view.updateAllHeaders();
            mssMenuPopulate();
            drawTrees();
            edit.doStack([edit.doUndeleteRow,[htmlrow,xmlrow,index]],doing);
        },

        doUndeleteRow: function(htmlrow,xmlrow,index,doing = 'do') {
            const label = xmlrow.getAttribute('n');
            const teis = [...Find.teis()];
            if(index === teis.length) {
                _state.xml.documentElement.appendChild(xmlrow);
                _state.matrix.boxdiv.querySelector('tbody').appendChild(htmlrow);
            }
            else {
                const trs = [...Find.trs()];
                _state.xml.documentElement.insertBefore(xmlrow,teis[index]);
                trs[index].parentNode.insertBefore(htmlrow,trs[index]);
            }
            view.updateAllHeaders();
            mssMenuPopulate();
            drawTrees();
            edit.doStack([edit.doDeleteRow,[label]],doing);
        },

        doMoveRow: function(movetr,appendafter,doing = 'do') {
            const table = movetr.parentNode;
            //const trs = [...table.children];
            const trs = [...Find.trs()];
            const oldsib = movetr.previousElementSibling;
            const previndex = trs.indexOf(movetr);
            const appendindex = appendafter !== null ?
                trs.indexOf(appendafter) :
                null;
            const HTMLMove = function() {
                if(appendafter === null)
                    table.insertBefore(movetr,table.firstChild);
                else if(appendafter.nextElementSibling)
                    table.insertBefore(movetr,appendafter.nextElementSibling);
                else
                    table.appendChild(movetr);
            };
            const XMLMove = function() {
                const root = _state.xml.documentElement;
                const teis = [...Find.teis()];
                const moverow = teis[previndex];
                if(appendindex === null)
                    root.insertBefore(moverow,teis[0]);
                else {
                    const appendxml = teis[appendindex];
                    if(appendxml.nextElementSibling)
                        root.insertBefore(moverow,appendxml.nextElementSibling);
                    else
                        root.appendChild(moverow);
                }
            };
            HTMLMove();
            XMLMove();
            if(doing === 'multido')
                return [edit.doMoveRow,[movetr,oldsib]];
            else
                edit.doStack([edit.doMoveRow,[movetr,oldsib]],doing);
        },

        doMerge: function(nums,doing = 'do') {
            /*        const merge = function(doc,selector,tag,attribute,nums) {
            const rows = doc.querySelectorAll(selector);
            var rowsclone = [];
            for(const row of rows) {
                const arr = [...nums].map(n => {
                        const cell = row.querySelector(tag+'['+attribute+'="'+n+'"]');
                        edit.unnormalize(cell);
                        return cell;
                    });
                const arrclone = arr.map(el => el.cloneNode(true));
                rowsclone.push(arrclone);
                const reduced = arr.reduce(function(acc,cur) {
                    if(cur.hasChildNodes()) {
                        const targ = cur.IAST ? cur.IAST : cur;
                        if(acc.hasChildNodes())
                            acc.appendChild(document.createTextNode(' '));
                        while(targ.firstChild)
                            acc.appendChild(targ.firstChild)
                    }
                    cur.parentNode.removeChild(cur);
                    return acc;
                    });
                reduced.normalize();
                reduced.IAST = reduced.cloneNode(true);
            }
            return [doc,selector,tag,attribute,rowsclone];
        } */
            const merge = function(rowfunc,cellfunc,nums) {
                const rows = rowfunc();
                var rowsclone = [];
                for(const row of rows) {
                    /*
                    const arr = [...nums].map(n => {
                        const cell = cellfunc(n,row);
                        edit.unnormalize(cell);
                        return cell;
                    });
                    */
                    const arr = [...nums].map(n => cellfunc(n,row));
                    const arrclone = arr.map(el => el.cloneNode(true));
                    rowsclone.push(arrclone);
                    for(const a of arr) edit.unnormalize(a);
                    const reduced = arr.reduce(function(acc,cur) {
                        if(cur.hasChildNodes()) {
                            while(cur.firstChild)
                                acc.appendChild(cur.firstChild);
                        }
                        cur.parentNode.removeChild(cur);
                        return acc;
                    });
                    reduced.normalize();
                    if(reduced.dataset) {
                        delete reduced.dataset.normal;
                        reduced.IAST = reduced.cloneNode(true);
                    }
                    else 
                        reduced.removeAttribute('lemma');
                }
                return [rowfunc,cellfunc,rowsclone];
            };
            //const oldhtml = merge(document,'.matrix tr','td','data-n',nums);
            //const oldxml = merge(_state.xml,'text','w','n',nums);
            const oldhtml = merge(Find.trs,Find.firsttd,nums);
            const oldxml = merge(Find.texts,Find.firstword,nums);
            const start = parseInt([...nums][0]);
            edit.renumber(start);
            //edit.renumber(document,'.matrix tr','td','data-n',start);
            //edit.renumber(_state.xml,'text','w','n',start);
            edit.restyleGroups(nums);
            //view.renormalize(start,start+1);
            edit.refresh();
            view.updateAllHeaders();
            //view.updateHeaders(nums);

            if(doing === 'multido')
                return [edit.doUnmerge,[oldhtml,oldxml]];
            else
                edit.doStack([edit.doUnmerge,[oldhtml,oldxml]],doing);
        },
    
        doUnmerge: function(oldhtml,oldxml,doing = 'undo') {
            /*        const unmerge = function(doc,parents,childs,attribute,oldels) {
            const nums = oldels[0].map(el => el.getAttribute(attribute));
            const firstn = nums[0];
            const rows = doc.querySelectorAll(parents);
            for(var n=0;n<rows.length;n++) {
                const lastchild = oldels[n].pop();
                const anchor = rows[n].querySelector(childs+'['+attribute+'="'+firstn+'"]');
                anchor.parentNode.replaceChild(lastchild,anchor);
                for(const cell of oldels[n]) 
                    lastchild.parentNode.insertBefore(cell,lastchild);
            }
            edit.renumber(doc,parents,childs,attribute,firstn);
            return nums;
        } */
            const unmerge = function(rowfunc,cellfunc,oldels) {
                const attr = Find.whichattr(oldels[0][0]);
                const nums = oldels[0].map(el => el.getAttribute(attr));
                const firstn = nums[0];
                const rows = rowfunc();
                for(var n=0;n<rows.length;n++) {
                    const lastchild = oldels[n].pop();
                    const anchor = cellfunc(firstn,rows[n]);
                    anchor.parentNode.replaceChild(lastchild,anchor);
                    for(const cell of oldels[n]) 
                        lastchild.parentNode.insertBefore(cell,lastchild);
                }
                return nums;
            };
            unmerge(...oldhtml);
            const nums = unmerge(...oldxml);
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const start = sortednums[0];
            edit.renumber(start);
            edit.reIAST(nums);
            //const end = parseInt(sortednums[sortednums.length-1])+1;
            //view.renormalize(start,end);
            edit.refresh();
            edit.restyleGroups(nums);
            view.updateAllHeaders();

            if(doing === 'multido')
                return [edit.doMerge,[nums]];
            else 
                edit.doStack([edit.doMerge,[nums]],doing);
        },
   
        doGroup: function(nums,doing = 'do') {
            const numarr = [...nums];
            const firstnum = numarr.shift();
        
            const texts = Find.texts();
            for(const text of texts) {
                //const cl = document.createElementNS('http://www.w3.org/1999/xhtml','cl');
                const cl = document.createElementNS('http://www.tei-c.org/ns/1.0','cl');
                const firstw = Find.firstword(firstnum,text);
                //const firstw = text.querySelector('w[n="'+firstnum+'"]');
                firstw.parentNode.insertBefore(cl,firstw);
                cl.appendChild(firstw);
                for(const num of nums)
                    cl.appendChild(Find.firstword(num,text));
                //cl.appendChild(text.querySelector('w[n="'+num+'"]'));
            }

            const lastnum = numarr.pop();
        
            for(const td of Find.tds(firstnum)) {
                td.classList.add('group-start');
            }
            for(const td of Find.tds(lastnum)) {
                td.classList.add('group-end');
            }
            for(const num of numarr) {
                for(const td of Find.tds(num)) {
                    td.classList.add('group-internal');
                }
            }
            
            for(const textbox of _state.textboxes)
                textbox.refresh();

            if(doing === 'multido')
                return [edit.doUngroup,[nums]];
            else edit.doStack([edit.doUngroup,[nums]],doing);
        },
   
        doUngroup: function(nums,doing = 'do') {
        //const texts = _state.xml.querySelectorAll('text');
            const texts = Find.texts();

            // ungroup xml
            for(const text of texts) {
                let cl;
                for(const num of nums) {
                    const word = Find.firstword(num,text);
                    //const word = text.querySelector('w[n="'+num+'"]');
                    if(!cl) cl = word.closest('cl');
                    cl.parentNode.insertBefore(word,cl);
                }
                cl.parentNode.removeChild(cl);
            }
        
            // ungroup html
            const tds = [...nums].map(
                n => [...Find.tds(n)]
            ).reduce((a,b) => a.concat(b),[]);
            for(const td of tds)
                td.classList.remove('group-start','group-internal','group-end');
        
            for(const textbox of _state.textboxes)
                textbox.refresh();

            if(doing === 'multido')
                return [edit.doGroup,[nums]];
            else
                edit.doStack([edit.doGroup,[nums]],doing);
        },
        
        doGroupWords: function() {
            let groupstart = 0;
            const todo = [];
            for(let n=0;n<=_state.maxlemma;n++) {
                const tds = Find.tds(n);
                const classtest = tds[0].classList; 
                if(classtest.contains('group-start') ||
                   classtest.contains('group-internal') ||
                   classtest.contains('group-end')) {
                    
                    groupstart = n+1;
                    continue;
                }

                let total = tds.length;
                let spaced = 0;
                if(n === _state.maxlemma) spaced = total;
                else {
                    for(const td of tds) {

                        const txt = td.IAST ? td.IAST.textContent : td.textContent;
                        //if(txt === '')
                        //    total--;
                        //else 
                        if(txt.slice(-1) === ' ')
                            spaced++;
                    }
                }

                if(spaced / total >= 0.5) {
                    if(groupstart === n) {
                        groupstart++;
                        continue;
                    }
                    else {
                        const range = Find.range(groupstart,n);
                        todo.push([edit.doGroup,[range]]);
                        groupstart = n+1;
                    }
                }
            }

            edit.doMulti(todo);
        },

        doRemoveCol: function(nums,doing = 'do') {
            const remove = function(rowfunc,cellfunc,nums) {
                const rows = rowfunc();
                var rowsclone = [];
                for(const row of rows) {
                    const arr = [...nums].map(n => {
                        const cell = cellfunc(n,row);
                        edit.unnormalize(cell);
                        return cell;
                    });
                    const arrclone = arr.map(el => el.cloneNode(true));
                    rowsclone.push(arrclone);
                    for(const td of arr)
                        td.parentNode.removeChild(td);
                }
                return [rowfunc,cellfunc,rowsclone];
            };
            //const oldhtml = remove(document,'.matrix tr','td','data-n',nums);
            //const oldxml = remove(_state.xml,'text','w','n',nums);
            const oldhtml = remove(Find.trs,Find.firsttd,nums);
            const oldxml = remove(Find.texts,Find.firstword,nums);
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const start = parseInt([...sortednums][0])-1;
            edit.renumber(start);
            //edit.renumber(document,'.matrix tr','td','data-n',start);
            //edit.renumber(_state.xml,'text','w','n',start);
            //view.renormalize(start-1,start);
            edit.refresh();
            edit.restyleGroups(nums,true);
            view.updateAllHeaders();

            if(doing === 'multido')
                return [edit.doUnremoveCol,[oldhtml,oldxml]];
            else
                edit.doStack([edit.doUnremoveCol,[oldhtml,oldxml]],doing);

        },

        doUnremoveCol: function(oldhtml,oldxml,doing) {
            const unremove = function(rowfunc,cellfunc,oldels) {
                const attr = Find.whichattr(oldels[0][0]);
                const nums = oldels[0].map(el => el.getAttribute(attr));
                const firstn = nums[0];
                const rows = rowfunc();
                for(var n=0;n<rows.length;n++) {
                    const anchor = cellfunc(firstn,rows[n]);
                    for(const cell of oldels[n])
                        anchor.parentNode.insertBefore(cell,anchor);
                }
                //edit.renumber(doc,parents,childs,attribute,firstn);
                return nums;
            };
            unremove(...oldhtml);
            const nums = unremove(...oldxml);
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const start = parseInt(sortednums[0])-1;
            edit.renumber(start);
            const highlitcells = document.querySelectorAll('.highlitcell');
            for(const cell of highlitcells)
                cell.classList.remove('highlitcell');
            edit.reIAST(nums);
            //const end = parseInt(sortednums[sortednums.length-1])+1;
            //view.renormalize(start,end);
            edit.refresh();
            edit.restyleGroups(nums,true);
            view.updateAllHeaders();

            if(doing === 'multido')
                return [edit.doRemoveCol,[nums]];
            else
                edit.doStack([edit.doRemoveCol,[nums]],doing);
        },

        doInsertCol: function(aftern,doing = 'do') {
            const insert = function(rowfunc,cellfunc,newfunc,aftern) {
                const rows = rowfunc();
                for(const row of rows) {
                    const cell = cellfunc(aftern,row);
                    const newcell = newfunc(aftern + 1);
                    cell.insertAdjacentElement('afterend',newcell);
                }
            };
            insert(Find.trs,Find.firsttd,make.emptycell,aftern);
            insert(Find.texts,Find.firstword,make.emptyword,aftern);
            edit.renumber(aftern);
            //view.renormalize(aftern,aftern+2);
            edit.refresh();
            edit.restyleGroups([aftern,aftern+1],true);
            view.updateAllHeaders();
            
            const arg = [aftern+1];
            if(doing === 'multido')
                return [edit.doRemoveCol,[arg]];
            else
                edit.doStack([edit.doRemoveCol,[arg]],doing);
        },

        doEmend: function(cellnum,rownum,doing = 'do') {
            const tr = [...Find.trs()][rownum];
            const td = Find.firsttd(cellnum,tr);
            td.dataset.emended = true;

            const text = Find.tei(rownum).querySelector('text');
            //const text = [...Find.texts()][rownum];
            const word = Find.firstword(cellnum,text);
            word.setAttribute('emended','true');
            if(doing === 'multido')
                return [edit.doUnemend,[cellnum,rownum]];
            else
                edit.doStack([edit.doUnemend,[cellnum,rownum]],doing);

        },

        doUnemend: function(cellnum,rownum,doing = 'do') {
            const tr = [...Find.trs()][rownum];
            const td = Find.firsttd(cellnum,tr);
            delete td.dataset.emended;

            const text = Find.tei(rownum).querySelector('text');
            //const text = [...Find.texts()][rownum];
            const word = Find.firstword(cellnum,text);
            word.removeAttribute('emended');
            if(doing === 'multido')
                return [edit.doEmend,[cellnum,rownum]];
            else
                edit.doStack([edit.doEmend,[cellnum,rownum]],doing);
        },

        doChangeCell: function(cellnum,rownum,content,doing = 'do') {
            const oldcontent = edit.xmlChangeCell(cellnum,rownum,content);
            const cell = edit.htmlChangeCell(cellnum,rownum,content);
            //view.renormalize(cellnum-1,cellnum+1,rownum);    
            edit.refresh();
            view.updateAllHeaders(true);
            events.textClick({target: cell});

            if(doing === 'multido')
                return [edit.doChangeCell,[cellnum,rownum,oldcontent]];
            else
                edit.doStack([edit.doChangeCell,[cellnum,rownum,oldcontent]],doing);
        },

        htmlChangeCell: function(cellnum,rownum,content) { // returns cell
            const row = Find.tr(rownum);
            //const row = [...Find.trs()][rownum];
            const cell = Find.firsttd(cellnum,row);
            //const row = document.querySelector('.matrix table')
            //                    .querySelectorAll('tr')[rownum];
            //const cell = row.querySelector('td[data-n="'+cellnum+'"]');
            edit.unnormalize(cell);
            cell.textContent = content;
            if(cell.IAST) cell.IAST = cell.cloneNode(true);
            return cell;
        },
    
        xmlChangeCell: function(cellnum,rownum,content) { // returns oldcontent
            const row = Find.tei(rownum).querySelector('text');
            //const row = [...Find.texts()][rownum];
            const cell = Find.firstword(cellnum,row);
            //const row = _state.xml.querySelectorAll('text')[rownum];
            //const cell = row.querySelector('w[n="'+cellnum+'"]');
            edit.unnormalize(cell);
            const oldcontent = cell.textContent;
            if(cell.childNodes.length === 0)
                cell.appendChild(document.createTextNode(content));
            else
                cell.textContent = content;
            return oldcontent;
        },

        doMarkAs: function(type,states,doing = 'do') {
            const nums = [...states.keys()];
            const oldstates = Find.attr(type,nums);
            for(const num of nums) {
                const cells = Find.tds(num);
                const words = Find.words(num);
                //const cells = document.querySelectorAll('.matrix table td[data-n="'+num+'"]');
                //const words = _state.xml.querySelectorAll('w[n="'+num+'"]');
                if(states.get(num) === true) {
                    for(const cell of cells) 
                        cell.dataset[type] = 'true';
                    for(const word of words)
                        word.setAttribute(type,'true');
                }
                else {
                    for(const cell of cells)
                        delete cell.dataset[type];
                    for(const word of words)
                        word.removeAttribute(type);
                }
                const checkbox = Find.checkbox(num,type);
                checkbox.checked = states.get(num);
            }
            if(doing === 'multido')
                return [edit.doMarkAs,[type,oldstates]];
            edit.doStack([edit.doMarkAs,[type,oldstates]],doing);
        }, 

        refresh: function() {
            /*
        var newcsvarr = [];
        for(const [key,value] of _texts) {
            const par = _state.xml.querySelector('[n="'+key+'"] text');
            const text = [...par.querySelectorAll('w')].map(w => w.innerHTML);
            newcsvarr.push([key,{desc: value.desc, text: text}]);
        }
        _texts = new Map(newcsvarr);
        for(const box of _state.textboxes)
            box.refresh();
*/
            for(const textbox of _state.textboxes)
                textbox.refresh();
            multi.rehighlight();
            if(!Check.anyhighlit())
                multi.clearTrees();
            else
                multi.repopulateTrees(...Find.lowhigh(Find.highlit()));
        },

        /*    renumber: function(doc,parents,childs,attribute,start=0) {
        const rows = doc.querySelectorAll(parents);
        for(const row of rows) {
            const els = row.querySelectorAll(childs);
            for(var n=parseInt(start)+1;n < els.length;n++)
                els[n].setAttribute(attribute,n);
        }
    },*/
        renumber: function(start=0) {
            const dorenumber = function(rowfunc,cellfunc,start) {
                const rows = rowfunc();
                for(const row of rows) {
                    const els = [...cellfunc(false,row)];
                    const attr = Find.whichattr(els[0]);
                    for(var n=parseInt(start)+1;n < els.length;n++)
                        els[n].setAttribute(attr,n);
                }
            };
            dorenumber(Find.trs,Find.tds,start);
            dorenumber(() => [true],Find.ths,start);
            dorenumber(Find.texts,Find.words,start);
            _state.maxlemma = Find.maxlemma();
        //_state.maxlemma = Find.firsttext().lastElementChild.getAttribute('n');
        },
    
        reIAST: function(nums) {
            const lemmata = [...nums].map(n => [...Find.lemmata(n)]).flat();
            for(const lemma of lemmata)
                lemma.IAST = lemma.cloneNode(true);
        },

        restyleGroups: function(ns,extend = false) {
            const pend = function(arr) {
                var newarr = [...arr];
                const prepend = parseInt(ns[0]) - 1;
                const postpend = parseInt(ns[ns.length-1]) + 1;
                newarr.unshift(prepend);
                newarr.push(postpend);
                return newarr;
            };        

            const nums = extend ? pend(ns) : ns;

            const changeClass = function(els,c_lass = false) {
                const classes = new Set(['group-start','group-internal','group-end']);
                if(c_lass) classes.delete(c_lass);

                for(const el of els) {
                    for(const c of classes)
                        el.classList.remove(c);
                    if(c_lass)
                        el.classList.add(c_lass);
                }
            };

            for(const num of nums) {
                const word = Find.firstword(num);
                const tds = Find.tds(num);
                if(tds.length === 0) continue;
                const cl = word.closest('cl');
                if(cl) {
                    if(word === cl.firstElementChild)
                        changeClass(tds,'group-start');
                    else if(word === cl.lastElementChild) {
                        changeClass(tds,'group-end');
                    }
                    else
                        changeClass(tds,'group-internal');
                }
                else
                    changeClass(tds);
            }
        },
        
        startRenormalize: function(nums) {
            const numss = nums === false ?
                Find.highlit() :
                nums;
            edit.doRenormalize(Math.min(...numss),Math.max(...numss));
        },

        doRenormalize: function(startnum, endnum, rownum=false, doing='do') {
            const showNormalized = Check.normalizedView();
            
            const changedrows = new Map();

            const htmlrows = [...Find.trs()];
            const xmlrows = [...Find.texts()];
            const rownums = rownum ? [rownum] : [...htmlrows.keys()];

            for(const r of rownums) {
                const changedrow = new Map();
                const xmlrow = xmlrows[r];
                const allwords = [...Find.words(false,xmlrow)];
                const firstword = Find.firstword(startnum,xmlrow);
                const lastword = Find.firstword(endnum,xmlrow);
                const startn = allwords.indexOf(firstword);
                const endn = allwords.indexOf(lastword);
                const words = allwords.slice(startn, endn + 1);

                const htmlrow = htmlrows[r];
                const alltds = [...Find.tds(false,htmlrow)];
                const tds = alltds.slice(startn, endn + 1);

                const unnormwords = words.map(w => w.textContent);
                const normwords = Normalizer(unnormwords);
                
                for(let n=0;n<normwords.length;n++) {
                    const word = words[n];
                    const normword = normwords[n];
                    const unnormword = unnormwords[n];
                    const td = tds[n];

                    const oldlemma = word.hasAttribute('lemma') ?
                        word.getAttribute('lemma') :
                        false;
                    if(oldlemma !== false) {
                        if(oldlemma === normword) continue;
                        else {
                            changedrow.set(td.dataset.n,oldlemma);
                            edit.unnormalize(word);
                            edit.unnormalize(td);
                        }
                    }
                    else changedrow.set(td.dataset.n,null);

                    if(normword !== unnormword) {
                        word.setAttribute('lemma',normword);
                        td.dataset.normal = normword;

                        if(showNormalized) td.textContent = normword;
                    }
                }
                changedrows.set(r,changedrow);
            }
            view.updateAllHeaders(true);
            view.xScrollToHighlit();

            edit.doStack([edit.doUnrenormalize,[changedrows]],doing);
        },
        
        doUnrenormalize: function(rowsmap,doing = 'undo') {
            const showNormalized = Check.normalizedView();
            const undomap = new Map();
            const htmlrows = [...Find.trs()];
            const xmlrows = [...Find.texts()];

            for(const [rownum, row] of rowsmap.entries()) {
                const htmlrow = htmlrows[rownum];
                const xmlrow = xmlrows[rownum];
                const undorowmap = new Map();

                for(const [n, normword] of row.entries()) {
                    const td = Find.firsttd(n,htmlrow);
                    const word = Find.firstword(n,xmlrow);

                    const undo = word.hasAttribute('lemma') ?
                        word.getAttribute('lemma') :
                        null;
                    undorowmap.set(n,undo);
                    
                    if(normword === null) {
                        edit.unnormalize(td);
                        edit.unnormalize(word);
                    }
                    else {
                        td.dataset.normal = normword;
                        word.setAttribute('lemma',normword);
                        if(showNormalized) td.textContent = normword;
                    }
                }
                undomap.set(rownum,undorowmap);
            }
            edit.doStack([edit.doUnrenormalize,[undomap]],doing);
        },

        unnormalize: function(cell) {
            if(cell.hasOwnProperty('IAST'))
                cell.textContent = cell.IAST.textContent;
            if(cell.dataset && cell.dataset.hasOwnProperty('normal')) {
                delete cell.dataset.normal;
            }
            if(cell.getAttribute('lemma'))
                cell.removeAttribute('lemma');
        },

    }; // end edit

    const view = {
        toggleNormalize: function() {
            if(Check.normalizedView())
                view.showUnnormalized();
            else
                view.showNormalized();
        },
        showNormalized: function(box) {
            if(box) // called from Box.show()
                box.updatescript();
            else {
                const par = document.getElementById('views');
                par.classList.add('normalized');
                for(const textbox of _state.textboxes)
                    textbox.updatescript();
                _state.matrix.updatescript();
                if(!Check.anyhighlit())
                    multi.clearTrees();
                else
                    multi.repopulateTrees(...Find.lowhigh(Find.highlit()));
            }

            view.updateAllHeaders(true);
            view.xScrollToHighlit();
        },
        showUnnormalized: function() {
            document.getElementById('views').classList.remove('normalized');
            _state.matrix.updatescript();
            
            for(const textbox of _state.textboxes)
                textbox.updatescript();
            
            if(!Check.anyhighlit())
                multi.clearTrees();
            else
                multi.repopulateTrees(...Find.lowhigh(Find.highlit()));

            view.updateAllHeaders(true);
            view.xScrollToHighlit();
        },
    
        toggleHeader: function() {
            const header = _state.matrix.boxdiv.querySelector('tr.header');
            if(header.style.display === 'none')
                header.style.display = 'table-row';
            else
                header.style.display = 'none';
        },

        updateHeaders(nums) {
            for(const num of nums) {
                const th = Find.firstth(num);
                const [count,unique] = Find.readings(num);
                const readspan = th.querySelector('span.readings');
                const readings = count < 2 ? count : `${count}(${unique.size})`;
                readspan.textContent = readings;
            }
        },
    
        updateAllHeaders(readingsonly = false) {
            const trs = [...Find.trs()];
            const trwalkers = trs.map(tr => Find.trWalker(tr));
            const tds = [...Find.tds(false,trs[0])];
            const ths = [...Find.ths()];
            const head = _state.matrix.boxdiv.querySelector('tr.header');
            const newTh = function() {
                const th = document.createElement('th');
                const span = document.createElement('span');
                span.classList.add('readings');
                th.appendChild(span);
                const form = document.createElement('form');
                form.innerHTML = '<div><input class="insignificant" type="checkbox">Insignificant</div><div><input class="binary" type="checkbox">Binary</div>';
                th.appendChild(form);
                head.appendChild(th);
                return th;
            };
        
            for(let n = 0;n<tds.length;n++) {
                const th = n >= ths.length ?
                    newTh() : ths[n];
                const td = tds[n];

                th.dataset.ref = td.dataset.n;
                let count = 0;
                const unique = new Set();
                for(const walker of trwalkers) {
                    const txt = walker.nextNode().textContent;
                    if(txt !== '') {
                        count++;
                        unique.add(txt);
                    }
                }
            
                const readings = count < 2 ? count : `${count}(${unique.size})`;
                th.querySelector('span.readings').textContent = readings;
                if(!readingsonly) {
                    th.querySelector('input.insignificant').checked = td.dataset.insignificant ? true : false;
                    th.querySelector('input.binary').checked = td.dataset.binary ? true : false;
                }
            }
            if(ths.length > tds.length) {
                for(let n=tds.length;n<ths.length;n++)
                    head.removeChild(ths[n]);
            }
        },
        xScrollToHighlit: function() {
            const hl = Find.highlit();
            if(hl.size > 0) view.xScroll([...hl][0]);
        },    
        xScroll: function(num,row) {
            if(!num) return;
            const par = row || Find.firsttr();
            const el = Find.firsttd(num,par);
            const elrect = el.getBoundingClientRect();
            const matrix = _state.matrix.boxdiv;
            const matrixrect = matrix.getBoundingClientRect();
            const rightboundary = matrixrect.right;
            const anchorrect = par.querySelector('th').getBoundingClientRect();
            const leftboundary = anchorrect.right;
            const outright = elrect.right > rightboundary;
            const outleft = (elrect.left + 0.1) < leftboundary;
            if(outright) el.scrollIntoView({inline: 'end', block: 'nearest'});
            if(outleft) {
                el.scrollIntoView({inline: 'start', block: 'nearest'});
                matrix.scroll({left: matrix.scrollLeft - anchorrect.width});
            }
        },
    };

    const make = {
        tei: function(label) {
            const tei = _state.xml.createElementNS(_const.teins,'TEI');
            tei.setAttribute('n',label);
            /*
            const text = _state.xml.createElementNS(_const.teins,'text');
            tei.appendChild(text);
            make.emptywords(text);
            */
            const template = Find.firsttext().cloneNode(true);
            tei.appendChild(template);
            for(const w of Find.words(false,template)) {
                w.removeAttribute('lemma');
                while(w.firstChild)
                    w.firstChild.remove();
            }
            return tei;
        },
        
        emptycell: function(n) {
            const td = document.createElement('td');
            td.className = 'lemma';
            td.dataset.n = n;
            return td;
        },

        emptyword: function(n,doc = _state.xml) {
            const w = doc.createElementNS(_const.teins,'w');
            w.setAttribute('n',n);
            return w;
        },

        emptywords: function(text,max,start) {
            const m = max || _state.maxlemma;
            const n_start = start || 0;
            for(let n = n_start; n <= m; n++) {
                const word = make.emptyword(n);
                text.appendChild(word);
            }
            

        },
        row: function(label,type) {
            const tr = document.createElement('tr');
            const th = document.createElement('th');
            th.scope = 'row';
            th.draggable = true;
            th.appendChild(document.createTextNode(label));
            th.addEventListener('dragstart',events.thDragStart);
            tr.dataset.n = label;
            tr.appendChild(th);
            const firstrow = Find.firsttr();
            for(const ftd of firstrow.querySelectorAll('td')) {
                const td = document.createElement('td');
                td.dataset.n = ftd.dataset.n;
                td.className = ftd.className;
                if(type) td.classList.add(type);
                tr.appendChild(td);
            }
            /*
            for(let n=0;n<=_state.maxlemma;n++) {
                const td = document.createElement('td');
                td.dataset.n = n;
                td.className = 'lemma';
                if(type) td.classList.add(type);
                tr.appendChild(td);
            }
            */
            return tr;
        },
    };

    const contextMenu = {

        create: function(e) {
            const contextmenu = document.createElement('div');
            contextmenu.classList.add('contextmenu');
            contextmenu.style.left = (e.clientX - 12) + 'px';
            contextmenu.style.top = (e.clientY - 22) + 'px';
            return contextmenu;
        },

        remove: function() {
            for(const oldmenu of document.querySelectorAll('.contextmenu'))
                oldmenu.parentNode.removeChild(oldmenu);
        },
        show: function(menu) {
            document.body.appendChild(menu);
        },
        populate: function(menu,items) {
            const list = document.createElement('ul');
            for(const item of items) {
                const li = document.createElement('li');
                if(item.hasOwnProperty('cond')) {
                    const frag = document.createRange().createContextualFragment(
                        '<form><input type="checkbox"'+(item.cond() ? ' checked' : '')+'></form>'
                    );
                    li.appendChild(frag);
                }

                if(item.hasOwnProperty('toggle')) {
                    const txt = item.toggle() ? item.text : item.alt;
                    li.appendChild(document.createTextNode(txt));
                }
                else
                    li.appendChild(document.createTextNode(item.text));
                li.addEventListener('mouseup',item.func);
                list.appendChild(li);
            }
            menu.appendChild(list);
        },
    };

    /*** Classes ***/

    class menuBox {
        constructor(name) {
            this.name = name;
            this.box = document.createElement('div');
            this.box.classList.add('menubox');
            const heading = document.createElement('div');
            heading.classList.add('heading');
            heading.appendChild(document.createTextNode(name));
            this.box.appendChild(heading);
            heading.addEventListener('mouseover',this.checkConditions.bind(this));
            this.box.addEventListener('mouseup',this.click.bind(this));
            this.items = new Map();
            this.conditions = new Map();
        }

        populate(items) {
            const ul = this.box.querySelector('ul') || 
            (function(obj) {
                const newul = document.createElement('ul');
                obj.box.appendChild(newul);
                return newul;})(this);
            for(const item of items) {
                const li = document.createElement('li');

                if(item.hasOwnProperty('checkbox')) {
                    const form = document.createElement('form');
                    const input = document.createElement('input');
                    input.type = 'checkbox';
                    input.addEventListener('click',e => e.preventDefault());
                    form.appendChild(input);
                    li.appendChild(form);
                    li.appendChild(document.createTextNode(item.text));
                    this.conditions.set(input,item.checkbox);
                }
                else if(item.hasOwnProperty('toggle')) {
                    const span = document.createElement('span');
                    span.appendChild(document.createTextNode(item.text));
                    span.dataset.text = item.text;
                    span.dataset.alt = item.alt;
                    this.conditions.set(span,item.toggle);
                    li.appendChild(span);
                }
                else
                    li.appendChild(document.createTextNode(item.text));
            
                if(item.hasOwnProperty('greyout')) {
                    this.conditions.set(li,item.greyout);
                }
            
                this.items.set(li,item.func);
                ul.appendChild(li);
            }
        }

        click(e) {
            const li = e.target.tagName === 'LI' ?
                e.target :
                e.target.closest('li');
            if(li && !li.classList.contains('greyedout')) {
                const func = this.items.get(li);
                if(func) {
                    func(e);
                    this.checkConditions();
                }
            }
        }

        checkConditions() {
            const checked = new Map();
            for(const [el, func] of this.conditions) {
                const result = checked.get(func) || 
                (function() {const x = func(); 
                    checked.set(func,x); 
                    return x;})();
                if(el.tagName === 'INPUT') {
                    el.checked = result;
                }
                else if (el.tagName === 'SPAN') {
                    if(result) 
                        el.textContent = el.dataset.alt;
                    else
                        el.textContent = el.dataset.text;
                }
                else if(el.tagName === 'LI') {
                    if(!result) el.classList.add('greyedout');
                    else el.classList.remove('greyedout');
                }
            }
        }
    }

    class Box {
        constructor(name) {
            this.name = name;
            this.script = 0;
        }
   
        show() {
            _state.descs.appendChild(this.descbox);
            if(Check.normalizedView())
                view.showNormalized(this);
            _state.viewdiv.appendChild(this.boxdiv);
            this.closed = false;
        }

        clear() {
            while(this.boxdiv.firstChild)
                this.boxdiv.removeChild(this.boxdiv.firstChild);
            if(this.svgcontainer) {
                this.clearsvg();
                this.boxdiv.appendChild(this.svgcontainer);
            
            }
        }

        destroy() {
            _state.viewdiv.removeChild(this.boxdiv);
            _state.descs.removeChild(this.descbox);
            const treeindex = _state.trees.indexOf(this);
            if(treeindex > -1)
                _state.trees.splice(treeindex,1);
            const textindex = _state.textboxes.indexOf(this);
            if(textindex > -1)
                _state.textboxes.splice(textindex,1);
            this.closed = true;
            //underlineVariants();
            if(this.name === 'Matrix')
                document.getElementById('matrixmenu').style.display = 'block';
            drawTrees();
            multi.rehighlight();
        }

        pullout() {
            this.destroy();
            const features = 'menubar=no,location=no,status=no,height=620,width=620,scrollbars=yes,centerscreen=yes';
            const slavenum = window.mainWindow ?
                window.mainWindow.comboView.getWindows().length :
                _state.windows.length;
            const newWindow = window.open('slave.html','slave'+slavenum,features);
            newWindow.mainWindow = window.mainWindow ?
                window.mainWindow :
                window;
            newWindow.startbox = this.text ?
            //{text: {name: this.name, map: this.textmap}} :
                {text: {name: this.name}} :
                {tree: this.name};
            newWindow.mainWindow.comboView.addWindow(newWindow);
        }

        makeDescBox() {
            const descbox = document.createElement('div');
            const closer = document.createElement('div');
            closer.classList.add('closer');
            closer.innerHTML = 'x';
            closer.title = 'close';
            /*      const opener = document.createElement('div');
      opener.classList.add('opener');
      opener.innerHTML = '^';
      opener.title = 'open in new window'; */
            const scripter = document.createElement('div');
            scripter.classList.add('scripter');
            scripter.innerHTML = 'A';
            scripter.title = 'change script';
            descbox.appendChild(closer);
            //descbox.appendChild(opener);
            descbox.appendChild(scripter);
            descbox.appendChild(document.createTextNode(this.desc));
            this.descbox = descbox;
            closer.addEventListener('click',this.destroy.bind(this));
            //opener.addEventListener('click',this.pullout.bind(this));
            scripter.addEventListener('click',this.cyclescript.bind(this));
        }

        cyclescript() {
            this.script = this.script + 1;
            if(this.script === _const.scripts.length)
                this.script = 0;
            const scripter = this.descbox.querySelector('.scripter');
            if(this.script === 0)
                scripter.innerHTML = 'A';
            else
                scripter.innerHTML = to[_const.scripts[this.script]]('a');
            if(_const.scripts[this.script] === 'grantha')
                scripter.classList.add('grantha');
            else scripter.classList.remove('grantha');
            this.updatescript();
        }
  
        updatescript() {
            const nodes = this.boxdiv.querySelectorAll('.lemma,.tree-lemma');
            for(const node of nodes) {
                const hasNormalized = node.dataset.hasOwnProperty('normal');
                if(!hasNormalized && node.textContent.trim() === '') continue;
                /*if(!node.hasOwnProperty('IAST'))
                node.IAST = node.cloneNode(true); */
                const tochange = (function() {
                    if(Check.normalizedView() && hasNormalized) {
                        const temp = document.createElement('span');
                        temp.appendChild(document.createTextNode(node.dataset.normal));
                        return temp;
                    }
                    else
                        return node.IAST.cloneNode(true);
                }());
                const newnode = this.script === 0 ?
                    tochange :
                    changeScript(tochange,_const.scripts[this.script]);
                node.innerHTML = '';
                while(newnode.firstChild)
                    node.appendChild(newnode.firstChild);
            }
            if(_const.scripts[this.script] === 'grantha') 
                this.boxdiv.classList.add('grantha');
            else this.boxdiv.classList.remove('grantha');
            if(this.boxdiv.classList.contains('matrix'))
                view.xScrollToHighlit();
        }
    }

    class TreeBox extends Box {
        constructor(stemmaid,id) {
            super(`#${stemmaid} #${id}`);
            this.stemmaid = stemmaid;
            this.id = id;
            this.nexml = _state.treelist.get(this.name).cloneNode(true);
            this.desc = this.nexml.querySelector('tree').getAttribute('label');
        }
        init() {
            this.makeDescBox();
            const treediv = document.createElement('div');
            treediv.classList.add('tree-box');
            var divid;
            var n = 0;
            do {
                divid = 'tree' + n;
                n++;
            } while(document.getElementById(divid));
            treediv.id = divid;
            this.boxdiv = treediv;
            this.boxdiv.addEventListener('mouseover',events.treeMouseover.bind(this));
            this.boxdiv.addEventListener('click',events.treeClick);
            this.svgcontainer = document.createElement('div');
            this.svgcontainer.id = this.boxdiv.id + 'container';
            this.boxdiv.appendChild(this.svgcontainer);
            this.boxdiv.myTree = this;

            //const parser = new DOMParser();
            ///this.nexml = parser.parseFromString(_state.treelist.get(this.name),'text/xml');
            this.calcPaths();
            this.jiggleroot();
            this.findLevels();
            this.labelInternal();
        }
        show() {
            _state.descs.appendChild(this.descbox);
            _state.viewdiv.appendChild(this.boxdiv);
        }

        jiggleroot() {
            const oldroot = this.nexml.evaluate('//nex:node[@root="true"]',this.nexml,this.nsResolver,XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
            const edges = this.nexml.evaluate('//nex:edge[@source="'+oldroot.id+'"]|//nex:edge[@target="'+oldroot.id+'"]',this.nexml,this.nsResolver,XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,null);
            if(edges.snapshotLength === 3) {
                var oldedge;
                var newsrctrgt;
                for(let i=0;i<edges.snapshotLength;i++) {
                    const thisedge = edges.snapshotItem(i);
                    const sourceid = thisedge.getAttribute('source');
                    const targetid = thisedge.getAttribute('target');
                    const sourcenode = this.nexml.evaluate('//nex:node[@id="'+sourceid+'"]',this.nexml,this.nsResolver,XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
                    const targetnode = this.nexml.evaluate('//nex:node[@id="'+targetid+'"]',this.nexml,this.nsResolver,XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
                    if(!sourcenode.hasAttribute('otu') && !targetnode.hasAttribute('otu')) {
                        oldedge = thisedge;
                        newsrctrgt = targetid === oldroot.id ?
                            'target' : 'source';
                        break;
                    }
                }
                if(oldedge) {
                    const newroot = this.nexml.createElementNS(oldroot.namespaceURI,'node');
                    newroot.id = 'fakeroot';
                    newroot.setAttribute('root','true');
                    oldroot.removeAttribute('root');
                    oldroot.parentElement.insertBefore(newroot,oldroot);
                    const newedge = this.nexml.createElementNS(oldroot.namespaceURI,'edge');
                    newedge.id = 'newrootedge';
                    newedge.setAttribute('length','0');
                    newedge.setAttribute('source','fakeroot');
                    newedge.setAttribute('target',oldroot.id);
                    oldroot.parentElement.insertBefore(newedge,oldedge);
            
                    oldedge.setAttribute(newsrctrgt,newroot.id);
                }
            }
        }
    
        findLevels() {
            const alledges = this.nexml.querySelectorAll('edge');
            const taxa = [...this.nexml.querySelectorAll('node[otu]')];
            const tree = this.nexml;
            this.levels = [taxa];

            const getNextLevel = function(curlevel,edges) {
                const ids = curlevel.map(t => t.id);
                const dups = new Map();
                const nodups = new Map();
                const usededges = [];
                for (const e of edges) {
                    const target = e.getAttribute('target');
                    const source = e.getAttribute('source');
                    const group = (() => {
                        /*if(ids.indexOf(target) !== -1)
                            return {ancestor: tree.querySelector(`node[id="${source}"]`),
                                child: tree.querySelector(`node[id="${target}"]`)};
                        else if(ids.indexOf(source) !== -1)
                            return {ancestor: tree.querySelector(`node[id="${target}"]`),
                                child: tree.querySelector(`node[id="${source}"]`)};
                        else
                            return null;*/
                        if(ids.indexOf(target) !== -1)
                            return {ancestor: tree.getElementById(source),
                                child: tree.getElementById(target)};
                        else if(ids.indexOf(source) !== -1)
                            return {ancestor: tree.getElementById(target),
                                child: tree.getElementById(source)};
                        else
                            return null;
                    })();
                    if(group !== null) {
                        if(nodups.has(group.ancestor)) {// duplicate
                            const othergroup = nodups.get(group.ancestor);
                            dups.set(group.ancestor,[othergroup.child, group.child]);
                            usededges.push(e);
                            usededges.push(othergroup.edge);
                        }
                        else nodups.set(group.ancestor,{child: group.child, edge: e});
                    }
                }
                const dupkeys = [...dups.keys()];
                const leftovers = [...nodups.keys()].reduce((acc,key) => {
                    if(dupkeys.indexOf(key) === -1)
                        acc.push(nodups.get(key).child);
                    return acc;
                },[]);
            
                const unusededges = [...edges].reduce((acc,e) => {
                    if(usededges.indexOf(e) === -1)
                        acc.push(e);
                    return acc;
                },[]);

                return {match: dups, remainder: [...new Set(leftovers)],edges: unusededges};
            };

            var curnodes = taxa;
            var curedges = alledges;
            do {
                const nextlevel = getNextLevel(curnodes,curedges);
                this.levels.push(nextlevel.match);
                curnodes = [...nextlevel.match.keys(),...nextlevel.remainder];
                curedges = nextlevel.edges;
            } while (curedges.length > 0);
        }

        labelInternal() {
            for(const node of this.nexml.querySelectorAll('node:not([label])'))
                node.setAttribute('label',node.id);
        }

        fitch1() {
            const firstpass = new Map();
            for(const taxon of this.levels[0]) {
                const label = taxon.getAttribute('label');
                const treelemma = this.boxdiv.querySelector(`span.tree-lemma[data-id="${label}"]`);
                const reading = treelemma ? [Find.htmlreading(treelemma,Check.normalizedView())] : [];
                firstpass.set(taxon,new Set(reading));
            }
            for(let m=1;m<this.levels.length;m++) { // start at 1 (after taxa)
                for(const [node,children] of this.levels[m]) {
                    const readings = children.map(node => firstpass.get(node));
                    const intersection = Find.setIntersection(...readings);
                    const result = intersection.size > 0 ?
                        intersection :
                        Find.setUnion(...readings);
                    firstpass.set(node,result);

                }
            }
            return firstpass;

        }

        fitch2(firstpass) {
            const taxa = [...this.nexml.querySelectorAll('node[otu]')];
            const secondpass = new Map();

            for(const [node] of this.levels[this.levels.length-1]) {
                secondpass.set(node,firstpass.get(node));
            }

            for(let n=this.levels.length-1;n>1;n--) {
                for(const [node,children] of this.levels[n]) {
                    const ancestral = secondpass.get(node);
                    for(const child of children) {
                        if(taxa.indexOf(child) !== -1)
                            continue;
                        const childreading = firstpass.get(child);
                        if(childreading.size === 1)
                            secondpass.set(child,childreading);
                        else {
                            const intersection = Find.setIntersection(ancestral,childreading);
                            const result = intersection.size > 0 ?
                                intersection :
                                childreading;
                            secondpass.set(child,result);
                        }
                    }
                }
            }
            return secondpass;
        }

        fitch() {
            const firstpass = this.fitch1();
            const formatOutput = function(m) {
                const output = [...m].map(str => str.trim() === '' ? '_' : str);
                return output.length === 1 ? output[0] : '{' + output.join(', ') + '}';
            };

            const secondpass = this.fitch2(firstpass);
            for(const [node,reading] of secondpass) {
                const htmlnode = this.boxdiv.querySelector(`span.internal[data-key="${node.id}"]`);
                htmlnode.dataset.reconstructed = formatOutput(reading);
            }
        }

        clearsvg() {
            while(this.svgcontainer.firstChild)
                this.svgcontainer.removeChild(this.svgcontainer.firstChild);
        }
    
        removecolors() {
            //const colored = this.nexml.evaluate('//nex:node[@color]',this.nexml,this.nsResolver,XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,null);

            const colored = this.nexml.querySelectorAll('node[color]');
            for(let i=0; i < colored.snapshotLength; i++)
                colored.snapshotItem(i).removeAttribute('color');
        }

        drawlines(nodes,color) {
            this.removecolors();
            if(nodes) {
            /*
            const nodearr = nodes.split(';');
            const edges = this.getPath(...nodearr
                        .map(s => s.replace(/[-_]/g,''))
                    ).path;
            
            */
                const edges = this.getPath(...nodes.split(';')).path;
                const nodeset = new Set();
                for(const edge of edges) {
                    nodeset.add(edge.getAttribute('target'));
                    nodeset.add(edge.getAttribute('source'));
                }
                for(const node of nodeset) {
                    const el = this.nexml.evaluate('//nex:node[@id="'+node+'"]',this.nexml,this.nsResolver,XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
                    //const edges = this.nexml.evaluate('//nex:edge[@source="'+node+'"]|//nex:edge[@target="'+node+'"]',this.nexml,this.nsResolver,XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,null);
                    if(/*edges.snapshotLength < 3 && */!el.getAttribute('root'))
                        el.setAttribute('color',color);
                }
            }
            const width = parseInt(window.getComputedStyle(this.svgcontainer,null).width) - 15; // -15 for vertical scrollbar
            const maxheight = parseInt(window.getComputedStyle(this.boxdiv,null).height) - 10;
            const height = (function() {
                if(maxheight < 600) return 600;
                //else if(maxheight < 800) return maxheight;
                //else return 800;
                else return maxheight;
            })();
            Smits.PhyloCanvas.Render.Style.line.stroke = 'rgb(162,164,170)';
            this.phylocanvas = new Smits.PhyloCanvas(
                {nexml: this.nexml, fileSource: true},
                this.svgcontainer.id,
                width,height,
                // 'circular'
            );
            const highlit = this.svgcontainer.querySelectorAll('path:not([stroke="#a2a4aa"])');
            for(const high of highlit) {
                high.style.strokeWidth = '2';
            }
        }
        draw() {
            this.clear();
            this.drawlines();
            this.makeLabels();
        }
    
        clearLabels() {
            for(const txt of this.boxdiv.firstChild.querySelectorAll('text')) {
                txt.parentElement.removeChild(txt);
            }
        }


        makeLabels() {
            const alltexts = [...Find.texts()];
            const texts = new Set(alltexts.map(el => el.parentNode.getAttribute('n')));
            const reconstructed = new Map(
                alltexts.filter(el =>
                    el.parentNode.hasAttribute('corresp') &&
                (el.parentNode.getAttribute('corresp') === this.name))
                    .map(el => [el.parentNode.getAttribute('select').replace(/^#/,''),el.parentNode.getAttribute('n')])
            );
            for(const txt of this.boxdiv.firstChild.querySelectorAll('text')) {
                const newEl = document.createElement('div');
                newEl.setAttribute('class','tree-div');
                const offleft = parseInt(txt.getAttribute('x') - 5);
                const offtop = parseInt(txt.getAttribute('y')) - 15;
                newEl.style.left = offleft + 'px';
                newEl.style.top = offtop + 'px';
                const key = txt.textContent.trim();//.replace(/[-_]/g,'');
                //            newEl.innerHTML =
                /*                (texts.has(key) ?
                    `<span class="witness inactive" data-key="${key}">${key}</span>` :
                    `<span class="internal" data-key="${key}">${key}</span>`)
                + '<span class="tree-lemma '+key+'" data-id="'+key+'"></span>';
*/
                if(texts.has(key))
                    newEl.innerHTML =
`<span class="witness inactive" data-key="${key}">${key}</span><span class="tree-lemma ${key}" data-id="${key}"></span>`;
                else if(key !== 'fakeroot') {
                    if(reconstructed.has(key))
                        newEl.innerHTML = `<span class="internal reconstructed" data-key="${key}" data-label="${reconstructed.get(key)}">${reconstructed.get(key)}</span><span class="tree-lemma invisible ${key}" data-id="${key}" data-label="${reconstructed.get(key)}"></span>`;
                    else
                        newEl.innerHTML = `<span class="internal" data-key="${key}">0</span>`;
                }
                else newEl.innerHTML = `<span class="internal" data-key="${key}"></span>`;
                //while(txt.firstChild)
                //    txt.removeChild(txt.firstChild);
                txt.parentElement.removeChild(txt);
                this.boxdiv.appendChild(newEl);
            }
        }
    
        clearlemmata() {
            for(const el of this.boxdiv.querySelectorAll('span.tree-lemma')) {
                el.innerHTML = '';
                if(el.dataset.hasOwnProperty('normal'))
                    delete el.dataset.normal;
                el.IAST = el.cloneNode(true);
            }
        }
    
        populate(n,m) {
            /*        for(const [key,value] of _texts)
            for(const el of this.boxdiv.getElementsByClassName(key)) {
                el.innerHTML = '';
                if(m)
                    el.appendChild(Xslt.transformString(
                        value.text.slice(n,parseInt(m)+1).join(' '),
                        proc));
                else
                    el.appendChild(Xslt.transformString(
                        value.text[n],
                        proc));
                el.IAST = el.cloneNode(true); // why was this commented out?
            } */
            const texts = Find.texts();
            for(const text of texts) {
                const key = text.parentNode.getAttribute('n');
                const el = this.boxdiv.querySelector(`span.tree-lemma[data-id="${key}"]`) || this.boxdiv.querySelector(`span.tree-lemma[data-label="${key}"]`);
                if(!el) continue;
                if(!el.hasOwnProperty('IAST')) el.IAST = el.cloneNode(true);
                el.IAST.innerHTML = '';
                if(m) {
                    const arr = [];
                    const normarr = [];
                    var emended = false;
                    for(let x=n;x<=m;x++) {
                        const word = Find.firstword(x,text);
                        arr.push(word.innerHTML);
                        if(word.hasAttribute('lemma'))
                            normarr[x-n] = word.getAttribute('lemma');
                        if(word.hasAttribute('emended')) emended = true;
                    }
                    el.IAST.appendChild(Xslt.transformString(arr.join(' ').replace(/\s+/g,' ').trim(),Xslt.sheets['tree']));
                    if(normarr.length !== 0) {
                        const newarr = arr.slice(0).map((e,i) =>
                            normarr.hasOwnProperty(i) ?
                                normarr[i] :
                                e
                        );
                        const temp = document.createElement('span');
                        temp.appendChild(Xslt.transformString(newarr.join(' ').replace(/\s+/g,' ').trim(),Xslt.sheets['tree']));
                        el.dataset.normal = temp.innerHTML;
                    }
                    if(emended) el.dataset.emended = true;
                    else if(el.dataset.hasOwnProperty('emended')) delete el.dataset.emended;
                }
                else {
                    const word = Find.firstword(n,text);
                    el.IAST.appendChild(Xslt.transformString(word.innerHTML,Xslt.sheets['tree']));
                    if(word.hasAttribute('lemma'))
                        el.dataset.normal = word.getAttribute('lemma');
                    else
                        delete el.dataset.normal;
                    if(word.hasAttribute('emended')) el.dataset.emended = true;
                    else if(el.dataset.hasOwnProperty('emended')) delete el.dataset.emended;
                }
                if(Check.normalizedView() && el.dataset.hasOwnProperty('normal'))
                    el.innerHTML = el.dataset.normal;
                else
                    el.innerHTML = el.IAST.innerHTML;
            }
            const inactive = this.boxdiv.querySelectorAll('.inactive');
            for(const label of inactive)
                label.classList.remove('inactive');
            this.fitch();
        }

        calcPaths() {
            this.nodes = [];
            this.paths = [];
            this.longest = {path:[]};
            this.nsResolver = this.nexml.createNSResolver(this.nexml.ownerDocument == null ? this.nexml.documentElement : this.nexml.ownerDocument.documentElement );
            const nodesSnapshot = this.nexml.evaluate('//nex:node[@label]',this.nexml,this.nsResolver,XPathResult.ORDERED_NODE_SNAPSHOT_TYPE);
            for(let i=0; i < nodesSnapshot.snapshotLength; i++)
                this.nodes.push(nodesSnapshot.snapshotItem(i));
            for(let i=0; i < this.nodes.length; i++) {
                const startnode = this.nodes[i];
                const startlabel = startnode.getAttribute('label');
                const startid = startnode.id;
                for(let j=i+1;j<this.nodes.length;j++) {
                    const endnode = this.nodes[j];
                    const endlabel = endnode.getAttribute('label');
                    const endid = endnode.id;
                    this.paths.push({nodes: [startlabel,endlabel],
                        path: this.pathFind(startid,endid)});
                }
            }
            for(const key of Object.keys(this.paths))
                if(this.paths[key].path.length > this.longest.path.length)
                    this.longest = this.paths[key];
        }

        pathFind(startid,endid,checked) {
            if(!checked) checked = [];
            const edges = this.nexml.evaluate('//nex:edge[@source="'+startid+'"]|//nex:edge[@target="'+startid+'"]',this.nexml,this.nsResolver,XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,null);
            var path;

            for(let i=0; i < edges.snapshotLength; i++) {
                const thisedge = edges.snapshotItem(i);
                path = [thisedge];
                if(checked.indexOf(thisedge.id) != -1) continue;

                checked.push(thisedge.id);
                const otherend = thisedge.getAttribute('source') !== startid ?
                    thisedge.getAttribute('source') :
                    thisedge.getAttribute('target');
                if(otherend === endid) return path;
                const othernode = this.nexml.evaluate('//nex:node[@id="'+otherend+'"]',this.nexml,this.nsResolver,XPathResult.FIRST_ORDERED_NODE_TYPE,null);
                if(othernode.singleNodeValue.hasAttribute('otu')) continue;
                else {
                    const nextlevel = this.pathFind(otherend,endid,checked);
                    if(nextlevel) return path.concat(nextlevel);
                    else continue;
                }
            }
            return false;
        }
    
        analyzeVariants(n,m) {
            /* arguments: n -- index of lemma in each witness of the _texts object
     * returns object of longest paths:
     *      keys are lemmata,
     *      value is either:
     *          an object with properties length (int), branch_length (float), and paths (array)
     *          or a string, the name of the witness, pointing to the normalized lemma
     */
            const lemmata = [];
            const aliases = [];

            const makeLgLemma = function(str) {
                if(!str.startsWith('<lg')) return str;

                return Xslt.transformString(str,Xslt.sheets['lg']).firstChild.data.slice(1);
            };

            const multiLemmaConcat = function(arr) {
                return arr.map(lemma => {
                    return makeLgLemma(lemma);
                }).join(' ')
                    .replace(/\s+/g,' ')
                    .trim();
            };

            const getReading = Check.normalizedView() ?
                function(n,text) {
                    const word = Find.firstword(n,text);
                    return word.hasAttribute('lemma') ?
                        word.getAttribute('lemma') :
                        word.textContent;
                } :
                function(n,text) {
                    return Find.firstword(n,text).textContent;
                };

            for(const text of Find.texts()) {
                const key = text.parentNode.getAttribute('n');
            
                // ignore reconstructions and texts not in current tree
                if(!this.nexml.querySelector(`otu[label="${key}"]`))
                    continue;

                const lemma = m ?
                    multiLemmaConcat(
                    //Array.from(Array(parseInt(m)-n+1).keys(), p => p+n)
                        Find.range(n,m).map(x => getReading(x,text))
                    ) :
                    makeLgLemma(getReading(n,text));
                if(lemma === '')
                    if(lemmata.hasOwnProperty(''))
                        lemmata[''].push(key);
                    else
                        lemmata[''] = [key];
                else { // normalization dealt with elsewhere now
                    if(lemmata.hasOwnProperty(lemma))
                        lemmata[lemma].push(key);
                    else
                        lemmata[lemma] = [key];
                    /*                const next_lemma = m ?
                    findNextLemma2(value.text,m) :
                    findNextLemma2(value.text,n);
                const clean = normalize(lemma,next_lemma);
                if(lemmata.hasOwnProperty(clean))
                    lemmata[clean].push(key)
                else lemmata[clean] = [key];

                if(clean !== lemma)
                    aliases[key] = clean;
 */
                }
            }
            const longestPaths = {};
            for(const lemma of Object.keys(lemmata)) {
                var longest = {length: 0, branch_length: 0, paths: []};
                if(lemmata[lemma].length === 1) {
                    longest = false;
                }
                else {
                    for(let i=0;i<lemmata[lemma].length;i++) {
                        for(let j=i+1;j<lemmata[lemma].length;j++) {
                            const path = this.getPath(lemmata[lemma][i],lemmata[lemma][j]);
                            if(!path.hasOwnProperty('path'))
                                //console.log(path);
                                alert(path);
                            if(path.path.length === longest.length) {
                                const branch_length = this.calcBranchLength(path.path);
                                if(branch_length === longest.branch_length)
                                    longest.paths.push(path);
                                else if(branch_length > longest.branch_length)
                                    longest = {length: path.path.length,
                                        branch_length: branch_length,
                                        paths: [path]};
                            }
                            else if(path.path.length > longest.length)
                                longest = {length: path.path.length,
                                    branch_length: this.calcBranchLength(path.path),
                                    paths: [path]};
                        }
                    }
                }
                longestPaths[lemma] = longest;
            }
            for(const key of Object.keys(aliases))
                longestPaths[key] = aliases[key];
            return longestPaths;
        }

        getPath(wit1,wit2) {
            for(const path of this.paths) {
            //const nodes = path.nodes.map(s => s.replace(/[-_]/g,''));
                if(path.nodes.indexOf(wit1) > -1 && path.nodes.indexOf(wit2) > -1)
                    return path;
            }
            return false;
        }

        calcBranchLength(path) {
            return path.map(node => node.getAttribute('length'))
                .reduce((acc,cur) => parseFloat(acc)+parseFloat(cur));
        }
    
        pickColour(fadeFraction, rgbColor1, rgbColor2, rgbColor3) {
        // from https://gist.github.com/gskema/2f56dc2e087894ffc756c11e6de1b5ed
            var color1 = rgbColor1;
            var color2 = rgbColor2;
            var fade = fadeFraction;

            // Do we have 3 colors for the gradient? Need to adjust the params.
            if (rgbColor3) {
                fade = fade * 2;

                // Find which interval to use and adjust the fade percentage
                if (fade >= 1) {
                    fade -= 1;
                    color1 = rgbColor2;
                    color2 = rgbColor3;
                }
            }

            const diffRed = color2.red - color1.red;
            const diffGreen = color2.green - color1.green;
            const diffBlue = color2.blue - color1.blue;

            const gradient = {
                red: parseInt(Math.floor(color1.red + (diffRed * fade)), 10),
                green: parseInt(Math.floor(color1.green + (diffGreen * fade)), 10),
                blue: parseInt(Math.floor(color1.blue + (diffBlue * fade)), 10),
            };

            return 'rgb(' + gradient.red + ',' + gradient.green + ',' + gradient.blue + ')';
        }

        colourizeVariants(n,m) {
            const paths = this.analyzeVariants(n,m);
            for(const el of this.boxdiv.querySelectorAll('span.tree-lemma')) {
                const path = paths.hasOwnProperty(el.dataset.id) ?
                    paths[paths[el.dataset.id]] :
                    paths[el.textContent];
                const red = {red: 252, green: 70, blue: 107};
                const blue = {red: 63, green: 94, blue: 251};
                if(path) {
                    el.style.color = this.pickColour(path.length/this.longest.path.length,blue,red);
                    el.dataset.length = path.length;
                    el.dataset.branch_length = path.branch_length;
                    el.dataset.nodes = path.paths[0].nodes.join(';');
                }
                else {
                    el.style.color = this.pickColour(1/this.longest.path.length,blue,red);
                    el.dataset.length = 0;
                    el.dataset.branch_length = 0;
                    el.dataset.nodes = '';
                }
                if(el.textContent.trim() === '') {
                    el.innerHTML = '<span lang="en">\u00a0\u00a0\u00a0</span>';
                    el.style.backgroundColor = el.style.color;
                }
                else el.style.backgroundColor = '';
            }

        }
    }

    class EdBox extends Box {
        //    constructor(name,arr) {
        constructor(name) {
            super(name);
            //        this.textmap = arr;
            //this.desc = arr.get(name).desc;
            this.desc = name;
            this.text = Find.firsttext(name);
        //this.text = arr.get(name).text;
        //this.name = name;
        }
        init() {
            this.makeTextBox();
            this.makeDescBox();
            this.descbox.style.maxWidth = '595px';
            this.descbox.style.paddingLeft = '5px';
            this.boxdiv.addEventListener('mouseup',events.textMouseup);
        }
    
        refresh() {
        //this.text = _texts.get(this.name).text;
        //this.text = Find.firsttext(this.name);
            this.boxdiv.innerHTML = '';
            this.boxdiv.appendChild(Xslt.sheets['lemma'].transformToFragment(this.text,document));
            //this.boxdiv.appendChild(XSLTransformElement(this.text,xslt_proc));
            touchUpNode(this.boxdiv);
            for(const lemma of Find.lemmata(false,this.boxdiv)) {
                lemma.IAST = lemma.cloneNode(true);
            }
            //this.boxdiv.appendChild(csvToFrag(this.text));
            this.updatescript();
        }

        makeTextBox() {
            const textbox = document.createElement('div');
            textbox.dataset.id = this.name; 
            textbox.classList.add('text-box');
            textbox.appendChild(Xslt.sheets['lemma'].transformToFragment(this.text,document));
            //textbox.appendChild(XSLTransformElement(this.text,xslt_proc));
            touchUpNode(textbox);
            for(const lemma of Find.lemmata(false,textbox))
                lemma.IAST = lemma.cloneNode(true);
            //textbox.appendChild(csvToFrag(this.text));
            //touchUp(textbox);
            this.boxdiv = textbox;
        }
    }

    class MatrixBox extends Box {
        constructor() {
            super(name);
            this.desc = 'Matrix';
            this.name = 'Matrix';
            this.makeDescBox();
            this.makeViewBox();
            this.descbox.style.maxWidth = '100vw';
            this.descbox.style.paddingLeft = '5px';
        }
        init() {
            this.makeTable();
        }
        makeViewBox() {
            const box = document.createElement('div');
            box.classList.add('matrix');
            box.dataset.id = this.name;
            this.boxdiv = box;
        }
        makeTable() {
            /*        const header = document.createElement('table');
        header.classList.add('header');
        _texts.forEach((value,key) =>  {
            const head = document.createElement('th');
            const row = document.createElement('tr');
            head.appendChild(document.createTextNode(value.desc));
            row.appendChild(head);
            header.appendChild(row);
        });
        */
            const scroller = document.createElement('div');
            scroller.classList.add('scroller');

            scroller.append(Xslt.sheets['matrix'].transformToFragment(_state.xml,document));
            //scroller.append(XSLTransformElement(_state.xml.documentElement,xslt_proc));
            for(const th of scroller.getElementsByTagName('th'))
                th.addEventListener('dragstart',events.thDragStart);

            scroller.addEventListener('dragenter',events.trDragEnter);
            scroller.addEventListener('dragleave',events.trDragLeave);
            scroller.addEventListener('dragover',e => e.preventDefault());
            scroller.addEventListener('drop',events.trDragDrop);
            scroller.addEventListener('mousedown',events.matrixMousedown);
            //this.boxdiv.append(header);
        
            const head = document.createElement('tr');
            head.classList.add('header');
            const firsttd = document.createElement('td');
            firsttd.classList.add('anchor');
            head.appendChild(firsttd);
            const trs = [...Find.trs(scroller)];
            const trwalkers = trs.map(tr => Find.trWalker(tr));
            const tds = Find.tds(false,trs[0]);

            for(const td of tds) {
                const th = document.createElement('th');
                th.dataset.ref = td.dataset.n;
                let count = 0;
                const unique = new Set();
                for(const walker of trwalkers) {
                    const node = walker.nextNode();
                    node.IAST = node.cloneNode(true);
                    const txt = node.textContent;
                    if(txt !== '') {
                        count++;
                        unique.add(txt);
                    }
                }
                const readings = count < 2 ? count : `${count}(${unique.size})`;
                const readspan = document.createElement('span');
                readspan.classList.add('readings');
                readspan.appendChild(document.createTextNode(readings));
                th.appendChild(readspan);
                const form = document.createElement('form');
                form.innerHTML = '<div><input class="insignificant" title="insignificant" type="checkbox"' + 
                             (td.dataset.insignificant ? 'checked' : '') +
                             '></div><div><input class="binary" title="binary" type="checkbox"'+ 
                             (td.dataset.binary ? 'checked' : '') +
                             '></div>';
                th.appendChild(form);
                head.appendChild(th);
            }

            const tbody = scroller.querySelector('tbody');
            tbody.insertBefore(head,tbody.firstChild);
            //head.addEventListener('click',events.matrixHeaderClick);
            this.boxdiv.append(scroller);
        }
    }

    return {
        slaveinit: function() {
            comboView.init();
            if(window.startbox !== undefined) {
                if(window.startbox.tree)
                    newBox.tree(window.startbox.tree.stemmaid,window.startbox.tree.id);
                else if(window.startbox.text)
                    newBox.text(window.startbox.text.name,window.startbox.text.map);
            }
        },
        maininit: function() {
            document.getElementById('comboview').style.display = 'block';
            comboView.init();
            fillSelector();
        },
        init: function() {
        /*
        _texts = new Map(
            VPTexts.map(o => {
            o[1].text = o[1].text.split(';');
            return o;
            })
        );*/
            _state.viewdiv = document.getElementById('views');
            _state.descs = document.getElementById('descs');
            _state.viewdiv.addEventListener('click',events.textClick);
            //        _state.viewdiv.addEventListener('mouseover',lemmaMouseover);
            document.addEventListener('keydown',events.keyDown,{capture: true});
            document.addEventListener('contextmenu',events.rightClick);
            document.addEventListener('mouseup',contextMenu.remove);
        },
        getWindows: function() {
            return _state.windows;
        },
        addWindow: function(win) {
            _state.windows.push(win);
        },
        getViewdiv: function() {
            return _state.viewdiv;
        },
        getTrees: function() {
            return _state.trees;
        },
    };

}());

window.addEventListener('load',window.comboView.maininit);
