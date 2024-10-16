import { Filter, Tags } from './filters.mjs';
import Sanscript from  './sanscript.mjs';

const convertFiles = (indocs, ids) => {
    return ids.map(id => {return {id: id, text: collectTexts(id, indocs)}; });
};

const collectTexts = (parid, indocs) => {
    const outtext = indocs.map(indoc => {
        const siglum = indoc.querySelector('idno[type="siglum"]')?.textContent;
        const els = [];
        if(siglum)
            els.push([siglum,
                indoc.querySelector(`text [*|id="${parid}"], text [corresp="${parid}"]`)
            ]);
        else {
            const msItems = indoc.querySelectorAll('msItem[*|id]');
            if(msItems.length === 0) return '';

            for(const msItem of msItems) {
                const textid = msItem.getAttribute('xml:id');
                els.push([textid,
                    indoc.querySelector(`text[corresp='#${textid}'] [corresp='${parid}'], [*|id='${parid}']`)
                ]);
            }
        }
        return els.map(el => textToFastt(...el) ).join('');
    });
    return outtext.join('');
};
const scripts = new Map([
    ['tamil', /[\u0b80-\u0bff]/u],
    ['devanagari', /[\u0900-\u097f]/u],
    ['bengali', /[\u0980-\u09ff]/u],
    ['telugu', /[\u0c00-\u0c7f]/u],
    ['malayalam',/[\u0d00-\u0d7f]/u]
]);

const changeScript = (str) => {
    const script = ((str,scripts) => {
        for(const [name,range] of scripts) {
            if(str.match(range)) return name;
        }
        return 'iast';
    })(str,scripts);
    return script !== 'iast' ?
        Sanscript.t(str,script,'iast').replaceAll(/\u200D/g,'') :
        str;
};

const textToFastt = (textid,xml) => {
    const apps = xml?.querySelectorAll('app');
    const vartexts = apps ? makeVarTexts(xml,apps) : '';
    const txt = xml ? changeScript(filterXml(xml)) : '';
    return txt ? `>${textid}\n${txt}${vartexts}\n` : '';
};

const makeVarTexts = (par, apps) => {
    const sigla = new Set();
    const listWit = par.ownerDocument.querySelector('listWit');
    if(listWit) {
        const wits = listWit.querySelectorAll('witness');
        if(wits)
            for(const wit of wits) sigla.add('#' + wit.getAttribute('xml:id'));
    }
    else {
        for(const app of apps) {
            const rdgs = app.querySelectorAll('rdg');
            for(const rdg of rdgs) {
                const wits = rdg.getAttribute('wit')?.split(/\s+/);
                if(wits)
                    for(const wit of wits) sigla.add(wit);
            }
        }
    }
    /*
    const sigla = [...apps].reduce((acc,cur) => {
        const wits = [...cur.querySelectorAll('rdg')].reduce((witacc, rdgcur) => {
            const witarr = rdgcur.getAttribute('wit')?.split(/\s+/) || [];
            return witacc.concat(witarr);
        },[]);
        for(const wit of wits) acc.add(wit);
        return acc;
        }, new Set());
    */
    const vartexts = [...sigla].sort().map(siglum => {
        const parclone = par.cloneNode(true);
        for(const app of parclone.querySelectorAll('app')) {
            const rdg = app.querySelector(`rdg[wit~='${siglum}']`);
            if(rdg)
                app.replaceWith(rdg);
            else {
                const lem = app.querySelector('lem');
                if(lem) app.replaceWith(lem);
                else app.remove();
            }
        }
        return {siglum: siglum.replace(/^#/,''), text: parclone};
    });

    const opts = new Map([ ['rdg',Filter.IgnoreTag] ]);
    
    return vartexts.map(vartext => {
        return `\n>${vartext.siglum}\n` + filterXml(vartext.text, opts);
    }).join('');
};

const filterXml = (xml, opts) => {
    var ret = '';
    const walker = xml.ownerDocument.createTreeWalker(xml,4294967295, {acceptNode() {return 1;} });
    let curnode = walker.currentNode;
    while(curnode) {
        if(curnode.nodeType === 1) {
            const filter = opts?.get(curnode.nodeName) ||
                           Tags.get(curnode.nodeName) || 
                           Filter.IgnoreTag;
            if(filter === Filter.Ignore) {
                walker.previousNode();
                curnode.remove();
            }
        }
        else if(curnode.nodeType === 3) {
            ret = ret + curnode.data;
        }
        curnode = walker.nextNode();
    }
    return ret.replace(/[\n\s]+/g,' ').trim();
};

export { convertFiles };
