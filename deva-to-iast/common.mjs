import Sanscript from  './sanscript.mjs';

const convertFiles = docs => {
    return docs.map(doc => convertDoc(doc));
};

const convertDoc = doc => {
    const el = doc.querySelector('TEI > text');
    changeScript(el);
    return doc;
};

const scripts = new Map([
    ['tamil', /[\u0b80-\u0bff]/u],
    ['devanagari', /[\u0900-\u097f]/u],
    ['bengali', /[\u0980-\u09ff]/u],
    ['telugu', /[\u0c00-\u0c7f]/u],
    ['malayalam',/[\u0d00-\u0d7f]/u]
]);

const devaStuff = str => {
    let ret = str;
    const consonants = [
        'क','ख','ग','घ','ङ',
        'च','छ','ज','झ','ञ',
        'ट','ठ','ड','ढ','ण',
        'त','थ','द','ध','न',
        'प','फ','ब','भ','म',     
        'य','र','ल','व',
        'श','ष','स','ह',
        'ळ','ऴ','ऱ','ऩ',
        'य़',
        'क़','ख़','ग़','ज़','झ़','फ़','ड़','ढ़','थ़','ध़','व़' // q qh ġ z zh f ṙ ṙh ṫh ḋh w
    ];
    const vowels = ['अ','आ', // a ā
            'इ','ई', // i ī
            'उ','ऊ', // u ū
            'ऋ','ॠ', // ṛ ṝ
            'ऌ','ॡ', // l̥ l̥̄
            'ऎ','ए','ऐ', // e ē ai
            'ऒ','ओ','औ' // o ō au
    ];
    const cjoin = consonants.join('');
    const aiauregex = new RegExp(`(?<=[${cjoin}])[इउ]`,'g');
    ret = ret.replaceAll(aiauregex,m => `_${m}`);
    const finalconstregex = new RegExp(`[${cjoin}]्$|[${cjoin}]्(?=\\s)`,'g');
    ret = ret.replaceAll(finalconstregex,m => `${m}_`);
    return ret;
};

const changeScript = (el) => {
    const str = el.innerHTML;
    const script = ((str,scripts) => {
        for(const [name,range] of scripts) {
            if(str.match(range)) return name;
        }
        return 'iast';
    })(str,scripts);

    const preproc = script === 'devanagari' ? devaStuff(str) : str;

    const newstr = script !== 'iast' ?
        Sanscript.t(preproc,script,'iast').replaceAll(/\u200D/g,'') :
        str;
    el.innerHTML = newstr;
};

export { convertFiles };
