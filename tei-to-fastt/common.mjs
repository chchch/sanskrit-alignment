import { Filter, Tags } from './filters.mjs';

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
                    indoc.querySelector(`text[corresp='#${textid}'] [corresp='${parid}'], text[corresp='#${textid}'] [*|id='${parid}']`)
                ]);
            }
        }
        return els.map(el => textToFastt(...el) ).join('');
    });
    return outtext.join('');
};

const textToFastt = (textid,xml) => {
    const apps = xml?.querySelectorAll('app');
    const vartexts = apps ? makeVarTexts(xml,apps) : '';
    const txt = xml ? filterXml(xml) : '';
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
            else
                app.replaceWith(app.querySelector('lem'));
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
