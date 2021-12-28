import {Sanscript} from './sanscript.mjs';

const Normalizer = (function() {
    
    const filters_slp1 = [
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
            search: 'Ê',
            replace: () => 'E'
        },
        {
            name: 'pṛṣṭhamātrā au',
            search: 'Ô',
            replace: () => 'O'
        },
        {
            name: 'candrabindu',
            search: 'm̐',
            replace: () => 'M'
        },
        {
            name: 'oṃkāras',
            search: 'oṁ',
            replace: () => 'oM'
        },
        /*{
            name: 'non-ASCII characters',
            search:'&#\\d+;',
            replace: () => ''
        },*/
        {
            name: 'additional punctuation',
            search: '[()\\[\\],;?!|_\\-=+\\d.\'\\/]+',
            replace: () => ''
        },
        {
            name: 'geminated aspirated consonants',
            search: '(?:kK|gG|cC|jJ|wW|qQ|tT|dD|pP|bB)',
            replace: (match) => match[0].slice(-1)
        },
        {
            name: 'geminated m after h',
            search: '(?:Mhm|hmm)',
            replace: () => 'hm'
        },
        {
            name: 'geminated t',
            search: '([rfi]|p[aA])tt|tt(?=[rvy]\\S)',
            replace: (match) => `${match[1]}t`
        },
        { 
            name: 'geminated consonants after r',
            search: '([rf]\\s*)([kgcjwqdpbRnmyvl])\\2{1,2}', 
            replace: (match) => `${match[1]}${match[2]}`
        },
        {
            name: 'final nasal variants',
            search: '(?:M[lSs]|nn)(?!\\S)',
            replace: () => 'n'
        },
    
        {
            name: 'internal nasal variants',
            search: '[mnNYR](?=[pPbBmdDtTnwWqQcCjJkKgG])',
            replace: () => 'M'
        },
        {
            name: 'final anusvāra variants', // A 8.4.59
            search: 'M?[mN](?!\\S)|n(?=\\s+[tdn])|Y(?=\\s+[jc])',
            replace: () => 'M'
        }, 
        {
            name: 'visarga aḥ before voiced consonants',
            search: '(?<!\\sB)(?:a[Hr]|[o])(?=\\s+[\'gGjJqQdDnbBmrylvh])', // ignore bho?
            replace: () => 'aH'
        },
        {
            name: 'visarga aḥ before vowels',
            search: 'aH(?=\\s+[AiIeuUof])',
            replace: () => 'a'
        },
        {
            name: 'visarga aḥ before unvoiced consonants and space + anusvāra',
            search: 'o\\s+(?=[kKcCwWtTpPszSM])',
            replace: () => 'aH a'
        },
        {
            name: 'visarga āḥ variants',
            search: 'AH(?=\\s+[aAiIeEuUogGjJqQdDbBnmyrlvh])',
            replace: () => 'A'
        },
        {
            name: 'other visarga variants',
            search: 'H?[rszS](?!\\S)',
            replace: () => 'H'
        },
        {
            name: 'avagrahas',
            search: '\'',
            replace: () => 'a'
        },
        {
            name: 'internal visarga variants',
            search: 'z(?=[kK])|s(?=s)',
            replace: () => 'H'
        },
        {
            name: 'final au/āv',
            search: 'Av(?!\\S)',
            replace: () => 'O'
        },
        {
            name: 'final su',
            search: '(?<=[sz])v(?=\\s+[aAiIuUoOeE])',
            replace: () => 'u'
        },
        {
            name: 'final i',
            search: 'i(?=\\s+[aAuUoOeE])',
            replace: () => 'y'
        },
        {
            name: 'kcch/kś',
            search: 'k(\\s*)(?:S|c?C)',
            replace: (match) => `k${match[1]}S`
        },
        {
            name: 'cś/tś',
            search: '[tc](\\s*)S',
            replace: (match) => `c${match[1]}C`
        },
        {
            name: 'cch/ch',
            search: '([aAiIuUeEoO])C',
            replace: (match) => `${match[1]}cC`
        },
        {
            name: 'final t + hi', // just catch most common case here
            search: 'd(\\s+)D(?=[iy](?:\\s|$))',
            replace: (match) => `t${match[1]}h`
        },
        {
            name: 'final t + voiced syllable', // different rule for t + h = ddh
            search: 'd(?=(?:\\s+[aAiIeEuUoOgGdDbByrv]|\\s*$))',
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
            search: 'y(?=\\s+[aAuUeEoO])',
            replace: () => 'i'
        },
        {
            name: 'bhd for bdh',
            search: 'Bd',
            replace: () => 'bD'
        }
    ];

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
        /*{
            name: 'non-ASCII characters',
            search:'&#\\d+;',
            replace: () => ''
        },*/
        {
            name: 'additional punctuation',
            search: '[()\\[\\],;?!|_\\-=+\\d.\'\\/]+',
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
            replace: (match) => `${match[1]}t`
        },
        { 
            name: 'geminated consonants after r',
            search: '([rṛ]\\s*)([kgcjṭḍṇtdnpbmyvl])\\2{1,2}', 
            replace: (match) => `${match[1]}${match[2]}`
        },
        {
            name: 'final nasal variants',
            search: '(?:ṃ[lṣs]|nn)(?!\\S)',
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

    const spaces = {
        none: {
            name: 'remove spaces',
            search: '\\s',
            replace: () => ''
        },
        collapse: {
            name: 'collapse spaces',
            search: '\\s+',
            replace: () => ' '
        }
    };

    const replaceAll = function(filter, strs) {
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
                    const taillen = match[0].length - splitat;
                    remainder = {length: taillen, text: newtail};
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

    const filterAll = function(strs) {
        let retstrs = strs.map(s => Sanscript.t(s,'iast','slp1'));
        for(const filter of [...filters_slp1,spaces.none]) {
            retstrs = replaceAll(filter,retstrs);
        }
        return retstrs.map(s => Sanscript.t(s,'slp1','iast'));
    };

    return filterAll;
})();

//console.log(Normalizer(['a','r','t','th','ī','s','ā','r','tth','o ','p','a','t','ś','a','l','ī','m','artthisārttho ','pagacchati']));
//console.log(Normalizer(['artthisārttho pārttho pārttho ','pārttho ','pagacchati']));

export {Normalizer};
