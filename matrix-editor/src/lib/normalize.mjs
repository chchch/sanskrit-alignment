import {Sanscript} from './sanscript.mjs';

const nasals = new Map([
    ['c','ñ'],
    ['t','n'],
    ['k','ṅ']
]);

const filters_slpish = [
    {
        name: 'additional punctuation',
        group: 'all',
        search: '[()\\[\\],;?!|¦_‘’“”·\\-–—―=+\\d.\\/]+',
        replace: () => ''
    },
    {
        name: 'ignore long/short e',
        group: 'tamil',
        search: 'ē',
        replace: () => 'e'
    },
    {
        name: 'ignore long/short o',
        group: 'tamil',
        search: 'ō',
        replace: () => 'o'
    },
    {
        name: 'insert glide after back vowels',
        group: 'tamil',
        search: '([aāuūoōO])\\s+([aāiīuūeēoōO])',
        replace: (match) => `${match[1]} v${match[2]}`
    },
    {
        name: 'insert glide after front vowels',
        group: 'tamil',
        search: '([iīeēE])\\s+([aāiīuūeēEoōO])',
        replace: (match) => `${match[1]} y${match[2]}`
    },
    {
        name: 'final -m sandhi variants',
        group: 'tamil',
        search: 'm(\\s*)([ckt])',
        replace: (match) => `${nasals.get(match[2])}${match[1]}${match[2]}`
    },
    {
        name: 'ignore puḷḷi',
        group: 'tamil',
        search: '[kṅcñṭtnpmyrlvḻḷṟṉ](?!\\s*[aāiīuūeēEoōOḵh])|[kṅcñṭtnpmyrlvḻḷṟṉ]$',
        replace: (match) => `${match[0]}a`
    },
    {
        name: 'valapalagilaka',
        search: 'ṙ',
        replace: () => 'r'
    },
    {
        name: 'pṛṣṭhamātrā e',
        search: 'ê',
        replace: () => 'e'
    },
    {
        name: 'pṛṣṭhamātrā o',
        search: 'ô',
        replace: () => 'o'
    },
    {
        name: 'pṛṣṭhamātrā ai',
        search: 'Ê',
        replace: () => 'E'
    },
    {
        name: 'pṛṣṭhamātrā au',
        search: 'Ô',
        replace: () => 'O'
    },
    {
        name: 'candrabindu as anusvāra',
        search: 'm̐',
        replace: () => 'ṃ'
    },
    /*
    {
        name: 'oṃkāras',
        search: 'õ',
        replace: () => 'oṃ'
    },
    */
    /*{
        name: 'non-ASCII characters',
        search:'&#\\d+;',
        replace: () => ''
    },*/
    {
        name: 'geminated aspirated consonants',
        search: '(?:kK|gG|cC|jJ|ṭṬ|ḍḌ|tT|dD|pP|bB)',
        replace: (match) => match[0].slice(-1)
    },
    {
        name: 'geminated m after h',
        search: '(?:ṃhm|hmm)',
        replace: () => 'hm'
    },
    {
        name: 'geminated t',
        search: '([rṛi]|p[aā])tt|tt(?=[rvy]\\S)',
        replace: (match) => match[1] ? `${match[1]}t` : 't'
    },
    { 
        name: 'geminated consonants after r',
        search: '([rṛ]\\s*)([kgcjṭḍdpbṛnmyvl])\\2{1,2}', 
        replace: (match) => `${match[1]}${match[2]}`
    },
    {
        name: 'final nasal variants',
        search: '(?:[ṃṅ][lśs]|nn)(?!\\S)',
        replace: () => 'n'
    },
    {
        name: 'internal nasal variants',
        search: '[mnṅñṇ](?=[pPbBmdDtTnṭṬḍḌcCjJkKgG])',
        replace: () => 'ṃ'
    },
    {
        name: 'final anusvāra variants', // A 8.4.59
        search: 'ṃ?[mṅ](?!\\S)|n(?=\\s+[tdn])|Y(?=\\s+[jc])',
        replace: () => 'ṃ'
    }, 
    {
        name: 'visarga aḥ before voiced consonants',
        search: '(?<!\\sB)(?:a[ḥr]|[o])(?=\\s+[\'gGjJḍḌdDnbBmrylvh])', // ignore bho?
        replace: () => 'aḥ'
    },
    {
        name: 'visarga aḥ before vowels',
        search: 'aḥ(?=\\s+[āiīeuūoṛ])',
        replace: () => 'a'
    },
    {
        name: 'visarga aḥ before unvoiced consonants and space + anusvāra',
        search: 'o\\s+(?=[kKcCṭṬtTpPśṣsṃ])',
        replace: () => 'aḥ a'
    },
    {
        name: 'visarga āḥ variants',
        search: 'āḥ(?=\\s+[aāiīeEuUogGjḍḌdDbBnmyrlvh])',
        replace: () => 'ā'
    },
    {
        name: 'other visarga variants',
        search: 'ḥ?[rśṣs](?!\\S)',
        replace: () => 'ḥ'
    },
    {
        name: 'superfluous avagrahas',
        search: '(\\S)\'+',
        replace: (match) => match[1]
    },
    {
        name: 'double avagrahas',
        search: '\'\'',
        replace: () => 'ā'
    },
    {
        name: 'avagrahas',
        search: '\'',
        replace: () => 'a'
    },
    {
        name: 'internal visarga variants',
        search: 'ṣ(?=[kK])|s(?=s)',
        replace: () => 'ḥ'
    },
    {
        name: 'final au/āv',
        search: 'āv(?!\\S)',
        replace: () => 'O'
    },
    {
        name: 'final su',
        search: '(?<=[ṣs])v(?=\\s+[aāiīuūoOeE])',
        replace: () => 'u'
    },
    {
        name: 'final i',
        search: 'i(?=\\s+[aāuūoOeE])',
        replace: () => 'y'
    },
    {
        name: 'kcch/kś',
        search: 'k(\\s*)(?:ś|c?C)',
        replace: (match) => `k${match[1]}ś`
    },
    {
        name: 'cś/tś',
        search: '[tc](\\s*)S',
        replace: (match) => `c${match[1]}C`
    },
    {
        name: 'cch/ch',
        search: '([aāiīuūeEoO])C',
        replace: (match) => `${match[1]}cC`
    },
    {
        name: 'final t + hi', // just catch most common case here
        search: 'd(\\s+)D(?=[iy](?:\\s|$))',
        replace: (match) => `t${match[1]}h`
    },
    {
        name: 'final t + voiced syllable', // different rule for t + h = ddh
        search: 'd(?=(?:\\s+[aāiīeEuūoOgGdDbByrv]|\\s*$))',
        replace: () => 't'
    },
    {
        name: 'final t + n/m',
        search: '([ai])n(?=\\s+[nm])',
        replace: (match) => `${match[1]}t`
    },
    {
        name: 'final t + c/j',
        search: 'j(?=\\s+j)|c(?=\\s+c)',
        replace: () => 't'
    },
    {    
        name: 'i/y + vowel',
        search: 'y(?=\\s+[aāuūeEoO])',
        replace: () => 'i'
    },
    {
        name: 'bhd for bdh',
        search: 'Bd',
        replace: () => 'bD'
    }
];
/*
const filters = [
    {
        name: 'valapalagilaka',
        search: 'ṙ',
        replace: () => 'r'
    },
    {
        name: 'short e',
        search: 'ẽ',
        replace: () => 'e'
    },
    {
        name: 'short o',
        search: 'õ',
        replace: () => 'o'
    },
    {
        name: 'pṛṣṭhamātrā e',
        search: 'ê',
        replace: () => 'e'
    },
    {
        name: 'pṛṣṭhamātrā o',
        search: 'ô',
        replace: () => 'o'
    },
    {
        name: 'pṛṣṭhamātrā ai',
        search: 'aî',
        replace: () => 'ai'
    },
    {
        name: 'pṛṣṭhamātrā au',
        search: 'aû',
        replace: () => 'au'
    },
    {
        name: 'candrabindu',
        search: 'm̐',
        replace: () => 'ṃ'
    },
    {
        name: 'oṃkāras',
        search: 'oṁ',
        replace: () => 'oṃ'
    },
//        {
//            name: 'non-ASCII characters',
//           search:'&#\\d+;',
//            replace: () => ''
//        },
    {
        name: 'additional punctuation',
        search: '[()\\[\\],;?!|¦_\\-–—―=+\\d.\\/]+',
        replace: () => ''
    },
    {
        name: 'geminated aspirated consonants',
        search: '([kgcjṭḍtdpb]){2}h',
        replace: (match) => `${match[1]}h` 
    },
    {
        name: 'geminated m after h',
        search: '(?:ṃhm|hmm)',
        replace: () => 'hm'
    },
    {
        name: 'geminated t',
        search: '([rṛi]|p[aā])tt|tt(?=[rvy]\\S)',
        replace: (match) => match[1] ? `${match[1]}t` : 't'
    },
    { 
        name: 'geminated consonants after r',
        search: '([rṛ]\\s*)([kgcjṭḍṇtdnpbmyvl])\\2{1,2}', 
        replace: (match) => `${match[1]}${match[2]}`
    },
    {
        name: 'final nasal variants',
        search: '(?:[ṃṅ][lṣs]|nn)(?!\\S)',
        replace: () => 'n'
    },

    {
        name: 'internal nasal variants',
        search: '[mnṅñṇ](?=[pbmdtnṭḍcjkg])',
        replace: () => 'ṃ'
    },
    {
        name: 'final anusvāra variants', // A 8.4.59
        search: 'ṃ?[mṅ](?!\\S)|n(?=\\s+[tdn])|ñ(?=\\s+[jc])',
        replace: () => 'ṃ'
    }, 
    {
        name: 'visarga aḥ before voiced consonants',
        search: '(?:a[ḥr]|[o])(?=\\s+[\'gjḍdnbmyrlvh])', // ignore bho?
        replace: () => 'aḥ'
    },
    {
        name: 'visarga aḥ before vowels',
        search: 'aḥ(?=\\s+[āiīeuūoṛ])',
        replace: () => 'a'
    },
    {
        name: 'visarga aḥ before unvoiced consonants and space + anusvāra',
        search: 'o\\s+(?=[kcṭtpśṣsṃ])',
        replace: () => 'aḥ a'
    },
    {
        name: 'visarga āḥ variants',
        search: 'āḥ(?=\\s+[aāiīeuūogjḍdnbmyrlvh])',
        replace: () => 'ā'
    },
    {
        name: 'other visarga variants',
        search: 'ḥ?[rśṣs](?!\\S)',
        replace: () => 'ḥ'
    },
    {
        name: 'superfluous avagrahas',
        search: '(\\S)\'+',
        replace: (match) => match[1]
    },
    {
        name: 'double avagrahas',
        search: '\'\'',
        replace: () => 'ā'
    },
    {
        name: 'avagrahas',
        search: '\'',
        replace: () => 'a'
    },
    {
        name: 'internal visarga variants',
        search: 'ṣ(?=k)|s(?=s)',
        replace: () => 's'
    },
    {
        name: 'final au/āv',
        search: 'āv(?!\\S)',
        replace: () => 'au'
    },
    {
        name: 'final su',
        search: '(?<=[sṣ])v(?=\\s+[aāiīuūoe])',
        replace: () => 'u'
    },
    {
        name: 'final i',
        search: 'i(?=\\s+[aāuūeo])',
        replace: () => 'y'
    },
    {
        name: 'kcch/kś',
        search: 'k(\\s*)(?:ś|c?ch)',
        replace: (match) => `k${match[1]}ś`
    },
    {
        name: 'cś/tś',
        search: '[tc](\\s*)ś',
        replace: (match) => `c${match[1]}ch`
    },
    {
        name: 'cch/ch',
        search: '([aāiīuūeo])ch',
        replace: (match) => `${match[1]}cch`
    },
    {
        name: 'final t + hi', // just catch most common case here
        search: 'd(\\s+)dh(?=[iy](?:\\s|$))',
        replace: (match) => `t${match[1]}h`
    },
    {
        name: 'final t + voiced syllable', // different rule for t + h = ddh
        search: 'd(?=(?:\\s+[aāiīeuūogdbyrv]|\\s*$))',
        replace: () => 't'
    },
    {
        name: 'final t + n/m',
        search: '([ai])n(?=\\s+[nm])',
        replace: (match) => `${match[1]}t`
    },
    {
        name: 'final t + c/j',
        search: 'j(?=\\s+j)|c(?=\\s+c)',
        replace: () => 't'
    },
    {    
        name: 'i/y + vowel',
        search: 'y(?=\\s+[aāuūeo])',
        replace: () => 'i'
    },
    {
        name: 'bhd for bdh',
        search: 'bhd',
        replace: () => 'bdh'
    }
];
*/
const spaces = {
    none: {
        name: 'remove spaces',
        group: 'all',
        search: '\\s',
        replace: () => ''
    },
    collapse: {
        name: 'collapse spaces',
        group: 'all',
        search: '\\s+',
        replace: () => ' '
    }
};

const replaceAll = function(filter, strs) {
    const vowels = ['a','ā','i','ī','u','ū','o','ō','O','e','ē','E','ṛ','ṝ','l̥','l̥̄'];
    const full = strs.join('');
    const newstrs = []; 
    const matches = [...full.matchAll(filter.search)];
    let start = 0;
    let remainder = null;
    for(const str of strs) {
        const end = start + str.length;
        let newstr = str;
        let offset = 0;

        if(remainder) {
            if(remainder.length <= str.length) {
                newstr = strSplice(str,0,remainder.length,remainder.text);
                offset = remainder.text.length - remainder.length;
                remainder = null;
            }
            else {
                const remhead = remainder.text.slice(0,str.length);
                newstr = remhead;
                const remtail = remainder.text.slice(str.length);
                remainder = {length: remainder.length - str.length, text: remtail};
                continue; // is this necessary?
            }
        }

        while(matches.length > 0  && matches[0].index < end) {
            const match = matches.shift();
            const matchstart = match.index - start + offset;
            const newtxt = filter.replace(match);

            if(match.index + match[0].length < end) {
                newstr = strSplice(newstr,matchstart,match[0].length,newtxt);
                offset = offset + newtxt.length - match[0].length;
            }
            else {
                const splitat = str.length - matchstart + offset;
                const newhead = newtxt.slice(0,splitat);
                newstr = newstr.slice(0,matchstart) + newhead;
                const newtail = newtxt.slice(splitat);

                if(vowels.includes(newtail)) { // add inherent vowel
                    newstr = newstr + newtail;
                }
                else {
                    const taillen = match[0].length - splitat;
                    remainder = {length: taillen, text: newtail};
                }
                // the while loop should end here
            }
        }
        newstrs.push(newstr);
        start = end;
    }
    return newstrs;
};

const strSplice = function(str,start,len,splice_in) {
    return str.slice(0,start) + splice_in + str.slice(start + len);
};

const filterAll = function(strs,tamil=false) {
    let retstrs = strs.map(s => Sanscript.t(s,'iast','slpish'));
    for(const filter of [...filters_slpish,spaces.none]) {
        if(tamil) {
            if(filter.group !== 'tamil' && filter.group !== 'all') continue;
        }
        else if(filter.group === 'tamil') continue;
        retstrs = replaceAll(filter,retstrs);
    }
    return retstrs.map(s => Sanscript.t(s,'slpish','iast'));
};

//console.log(Normalizer(['a','r','t','th','ī','s','ā','r','tth','o ','p','a','t','ś','a','l','ī','m','artthisārttho ','pagacchati']));
//console.log(Normalizer(['artthisārttho pārttho pārttho ','pārttho ','pagacchati']));

export default filterAll;
