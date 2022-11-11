import { Filter, Tags } from './filters.mjs';

const convertFiles = (indocs, ids) => {
    return ids.map(id => {return {id: id, text: collectTexts(id, indocs)} });
};

const collectTexts = (id, indocs) => {
    const outtext = indocs.map(indoc => {
        const siglum = indoc.querySelector('idno[type="siglum"]')?.textContent;
        const texts = [...indoc.querySelectorAll('text')];
        return texts.map(text => textToFastt(text,siglum,id)).join('');
    });
    return outtext.join('');
};

const textToFastt = (par,siglum,id) => {
    const corresp = par.getAttribute('corresp')?.replace(/^#/,'') || siglum;
    const xml = par.querySelector(`[*|id="${id}"],[corresp="#${id}"]`);
    const apps = xml?.querySelectorAll('app');
    const vartexts = apps ? makeVarTexts(xml,apps) : '';
    const txt = xml ? filterXml(xml) : '';
    return txt ? `>${corresp}\n${txt}${vartexts}\n` : '';
};

const makeVarTexts = (par, apps) => {
    const sigla = new Set();
    // TODO: use listWit if available, adding # to xml:id
    const listWit = par.ownerDocument.querySelector('listWit');
    if(listWit) {
        const wits = listWit.querySelectorAll('witness');
        if(wits)
            for(const wit of wits) sigla.add(wit.getAttribute('xml:id'));
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
    const walker = xml.ownerDocument.createTreeWalker(xml,4294967295, {acceptNode() {return 1}});
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
