const comboView = (function() {

    'use strict';

    var _filename;
    var _xml;
    const _teins = 'http://www.tei-c.org/ns/1.0';
    //var _xml = document.createElementNS('http://www.w3.org/1999/xhtml','teiCorpus');
    //var _xml = document.createElementNS('http://www.tei-c.org/ns/1.0','group');
    //var _xml = document.createElementNS('http://www.w3.org/1999/xhtml','group');
    //var _texts = [];
    //var _editions = [];
    var _treelist = new Map();
    var _trees = [];
    var _textboxes = [];
    var _matrix;
    //var _menus;
    var _viewdiv;
    var _descs;
    var _maxlemma;
    var _windows = [window];
    var _dragged;
    var _undo = [];
    var _redo = [];
    var _editing = false;
    var _normalization = false;
    const _scripts = ['iast','devanagari','telugu','grantha','malayalam'];

    /*** Pure functions ***/

    const makeLgLemma = function(str) {
        if(!str.startsWith('<lg')) return str;

        const xslt_proc = makeXSLTProc(lgXSLT);
        return XSLTransformString(str,xslt_proc).firstChild.data.slice(1);
    };

    const multiLemmaConcat = function(arr) {
        return arr.map(lemma => {
            return makeLgLemma(lemma);
        }).join(' ')
            .replace(/\s+/g,' ')
            .trim();
    };

    const findNextLemma = function(lemma) {
        let nextlemma = lemma.nextElementSibling;
        while(nextlemma) {
            if(!lemma.classList.contains('invisible'))
                return lemma.textContent;
            nextlemma = nextlemma.nextElementSibling;
        }
        return '';
    };

/*    const findNextLemma2 = function(arr,n) {
        for(let i=parseInt(n)+1;i<arr.length;i++)
            if(arr[i] != '') return arr[i];
    };
*/
    const normalize = function(lemma,next,both=false) {
        const concated = lemma+';'+next;
        const clean1 = concated
        // <del>
            .replace(/<del\b(?:"[^"]*"|'[^']*'|[^'">])*>.*?<\/del>/g,'')
        // other tags
            .replace(/<\/?\w+?\b(?:"[^"]*"|'[^']*'|[^'">])*?\/?>/g,'')
        // punctuation
            .replace(/\s*[|,?—―-]/g,'');
        // numbers
        // .replace(/\d/g,'')

        const clean1split = clean1.split(';');
        if(clean1split[0] === 'bho' || clean1split[0] === 'bhobho')
            return clean1split;

        const arr = clean1
        // geminated consonants after r
            .replace(/([rṛi];?)([kgcjṭḍṇdnpbmyvl])\2{1,2}/,'$1$2')
        // geminated t
            .replace(/([rṛri];?|pa;?)tt/,'$1t')
            .replace(/tt(?=;?[rvy])/,'t')
        // final nasal variants
            .replace(/(?:ṃ[lśs]|nn)(?=[;\s])/,'n')
        // internal nasal variants
        //.replace(/[mnñṇṅ](?=[pbmdtnṭḍcjkg])/,'ṃ')
            .replace(/ṃ(?=;?[pbm])/,'m')
            .replace(/ṃ(?=;?[dtn])/,'n')
            .replace(/ṃ(?=;?[ṭḍṇ])/,'ṇ')
            .replace(/ṃ(?=;?[cj])/,'ñ')
            .replace(/ṃ(?=;?[kg])/,'ṅ')
        // visarga āḥ variants
            .replace(/ā(;?)ḥ(?=[;\s][aāiīeuūogjḍdbnmyrlvh])/,'ā$1')
        // visarga aḥ before unvoiced consonants and avagraha
            .replace(/o([;\s])(?=[kcṭtpśṣs'])/,'aḥ$1a')
        // visarga aḥ before voiced consonants
            .replace(/(?:aḥ|aḥ?r|o)(?=[;\s][gjḍdbnmrylvh])/,'aḥ')
        // saḥ before voiced consonants
            .replace(/^(s;?a;?)ḥ(?=[;\s][gjḍdbnmrylvh])/,'$1')
        // visarga aḥ before vowels other than short a
            .replace(/(a;?)ḥ(?=[;\s][āiīeuūoṛ])/,'$1')
        // other visarga variants
            .replace(/ḥ?[rsśṣ](?=[;\s])/,'ḥ')
        // internal visarga variants
            .replace(/([ui])[sṣ](?=[sk])/,'$1ḥ')
        // final anusvāra variants
            .replace(/ṃ?[mṅ](?=;)|ñ(?=[;\s][jc])/,'ṃ')
        // cch/ch
            .replace(/c(;?ch)/,'$1')
        // t + ś
            .replace(/c([;\s])ch/,'t$1ś')
        // final (a)t + voiced syllable, n/m, c, or j
            .replace(/d(?=[;\s][aāiīuūeogdbyrv])/,'t')
            .replace(/([ai];?)n(\s;?)(?=[;\s][nm])/,'$1t$2')
            .replace(/([^āai];?)t(\s;?)(?=[nm])/,'$1n$2')
            .replace(/j(\s;?)(?=j)/,'t$1')
            .replace(/c(\s;?)(?=c)/,'t$1')
        // intial chr
            .replace(/^chr/,'śr')
        // i/y + vowel
            .replace(/([^aāiīuūeo];?)y(?=[;\s][āauūeo])/,'$1i')
        // ḻ/l
            .replace(/ḻ/,'l')
        // -ena + u-
            .replace(/ena u/,'eno')
        // -sya + u-
            .replace(/sya u/,'syo')
        // other avagrahas
            .replace(/'/g,'')
            .split(';');
        const withnums = arr[0].trim();
        const nonums = withnums.replace(/\d/g,'');
        if(both) {
            const second = arr[1].trim();
            const nonums2 = second.replace(/\d/g,'');
            return [nonums === '' ? withnums : nonums,
                nonums2 === '' ? second : nonums2];
        }
        else
            return nonums === '' ? withnums : nonums;
    };

    const pickColour = function(fadeFraction, rgbColor1, rgbColor2, rgbColor3) {
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
    };

    const to = {

        smush: function(text,placeholder) {
            return text.toLowerCase()
        
            // remove space between a word that ends in a consonant and a word that begins with a vowel
                .replace(/([ḍdrmvynhs]) ([aāiīuūṛeoéó])/g, '$1$2'+placeholder)
        
            // remove space between a word that ends in a consonant and a word that begins with a consonant
                .replace(/([kgcjṭḍtdpb]) h/g, '$1\u200C'+placeholder+'h')
                .replace(/([kgcjñḍtdnpbmrlyvśṣsṙ]) ([kgcjṭḍtdnpbmyrlvśṣshḻ])/g, '$1'+placeholder+'$2')

            // join final o/e/ā and avagraha/anusvāra
                .replace(/([oóeéā]) ([ṃ'])/g,'$1'+placeholder+'$2')

                .replace(/^ṃ/,'\'\u200Dṃ') // initial anusvāra
                .replace(/^ḥ/,'\'\u200Dḥ') // initial visarga
                .replace(/^_y/,'\'\u200Dy') // half-form of ya
                .replace(/ü/g,'\u200Cu')
                .replace(/ï/g,'\u200Ci')

                .replace(/_{1,2}(?=\s*)/g, function(match) {
                    if(match === '__') return '\u200D';
                    else if(match === '_') return '\u200C';
                });
        },

        iast: function(text,f) {
            const from = f || 'devanagari';
            return Sanscript.t(text,from,'iast',{skip_sgml: true});
        },

        devanagari: function(text,p) {

            const placeholder = p || '';
            const options = {skip_sgml: true};

            const presmush = text.replace(/ṙ/g, 'r')
                .replace(/^_ā/,'\u093D\u200D\u093E');

            const smushed = to.smush(presmush,placeholder);

            const in_deva = Sanscript.t(smushed,'iast','devanagari',options);

            return in_deva.replace(/¯/g, 'ꣻ');
        },
    
        malayalam: function(text,p) {

            const placeholder = p || '';
            const options = {skip_sgml: true};
	
            const chillu = {
                'ക':'ൿ',
                'ത':'ൽ',
                'ന':'ൻ',
                'മ':'ൔ',
                'ര':'ർ',
            };

            const presmush = text.replace(/^_ā/,'\u0D3D\u200D\u0D3E');

            const smushed = to.smush(presmush,placeholder)
                .replace(/e/g,'ẽ') // hack to make long e's short
                .replace(/o/g,'õ') // same with o
                .replace(/ṙ/g,'r') // no valapalagilaka
                .replace(/ṁ/g,'ṃ') // no malayalam oṃkāra sign
                .replace(/ḿ/g,'ṃ')
                .replace(/í/g,'i') // no pṛṣṭhamātras
                .replace(/ú/g,'u')
                .replace(/é/g,'e'); 

            const in_mlym = Sanscript.t(smushed,'iast','malayalam',options);
	
            // use dot reph
            return in_mlym.replace(/(^|[^്])ര്(?=\S)/g,'$1ൎ')
        
            // use chillu final consonants	
                .replace(/([കതനമര])്(?![^\s\u200C,—’―])/g, 
                    function(match,p1) {
                        return chillu[p1];
                    }
                );
        },
    
        grantha: function(text,p) {

            const placeholder = p || '';
            const options = {skip_sgml: true};
            const finals = new Map([
                ['സ','\ue1d3'],
                ['ര','\u0d7c'],
                ['യ','\ue1cb'],
                ['ച','\ue1b6'],
                ['ദ','\ue1c2'],
                ['ശ','\ue1d2'],
            ]);

            // use classical final consonants	
            const finals_regex = new RegExp('(['+[...finals.keys()].join('')+'])്(?![^\s\u200C,—’―])','g'); 

            const presmush = text.replace(/^_ā/,'\u0D3D\u200D\u0D3E');

            const smushed = to.smush(presmush,placeholder)
                .replace(/e/g,'ẽ') // hack to make long e's short
                .replace(/o/g,'õ') // same with o
                .replace(/ṙ/g,'r') // no valapalagilaka
                .replace(/ṁ/g,'ṃ') // no malayalam oṃkāra sign
                .replace(/ḿ/g,'ṃ')
                .replace(/í/g,'i') // no pṛṣṭhamātras
                .replace(/ú/g,'u')
                .replace(/é/g,'e'); 

            const in_gnth = Sanscript.t(smushed,'iast','malayalam',options);
	
            // use dot reph (post-consonantal reph in grantha)
            return in_gnth.replace(/(^|[^്])ര്(?=\S)/g,'$1ൎ')
                .replace(finals_regex, (match,p1) => finals.get(p1))
    
            // use classical tamil kṣi
                .replace(/ക്ഷി/g,'க்ஷி');
        },

        telugu: function(text,p) {

            var placeholder = p || '';
            const options = {skip_sgml: true};

            const presmush = text.replace(/^_ā/,'\u0C3D\u200D\u0C3E');

            const smushed = to.smush(presmush,placeholder)
                .replace(/e/g,'ẽ') // hack to make long e's short
                .replace(/o/g,'õ') // same with o
                .replace(/ṙ/g,'r\u200D') // valapalagilaka
                .replace(/ṁ/g,'ṃ') // no telugu oṃkāra sign
                .replace(/ḿ/g,'ṃ')
                .replace(/í/g,'i') // no pṛṣṭhamātras
                .replace(/ú/g,'u')
                .replace(/é/g,'e');

            return Sanscript.t(smushed,'iast','telugu',options);
        },
    };

    /*** Pure-ish functions ***/

    const changeScript = function(orignode,script,placeholder = false,cur_lang = 'sa') {
        const func = to[script];
        const node = orignode.cloneNode(true);

        const loop = function(node,cur_lang) { 
            let kids = node.childNodes;

            for(let kid of kids) {
            
                if(kid.nodeType === 8) continue;

                if(kid.nodeType === 3) {
                    if(cur_lang !== 'sa')
                        continue;
                    else
                        kid.data = func(kid.data,placeholder);
                }
                else if(kid.hasChildNodes()) {
                    let kidlang = kid.getAttribute('lang') || cur_lang;
                    if(kidlang === 'sa' && kid.classList.contains('subst'))
                        jiggle(kid,script);
                    loop(kid,kidlang);
                }
            }
        }; //end loop function

        loop(node,cur_lang);
        return node;
    };
    /*
const jiggle = function(node,script) {
    const kids = node.childNodes;
    if(kids[0].nodeType !== 3 && kids[kids.length-1].nodeType !== 3) return;

    const initial_vowels_allowed = (kids[0].nodeType !== 3) ? true : false;
    var add_at_beginning = [];
    const vowels = ['ā','i','ī','u','ū','e','o','ṃ','ḥ','ai','au'];
    const vowels_regex = /[aāiīuūeoṃḥ_]$/;
    const cons_regex = /[kgṅcjñṭḍṇtdnpbmyrlvṣśsh]$/;

    const telugu_vowels = ['ā','i','ī','e','o','_','ai','au'];
    const telu_cons_headstroke = ['h','k','ś','y','g','gh','c','ch','jh','ṭh','ḍ','ḍh','t','th','d','dh','n','p','ph','bh','m','r','l','v','ṣ','s'];
    var telugu_del_headstroke = false;
    var telugu_kids = [];
    
    for (let kid of kids) {
        let txt = kid.textContent;
        if(kid.nodeType === 3) {
            if(txt.trim() === '') continue;
            else if(txt === 'a')
                kid.textContent = '';
            else if(vowels.indexOf(txt) >= 0) {
                let cap = document.createElement('span');
                cap.setAttribute('class','aalt');
                cap.appendChild(kid.cloneNode(false));
                node.replaceChild(cap,kid);
                kid = cap;
            }            
            else if(!txt.trim().match(vowels_regex)) {
                if(script === 'telugu' &&
                   telu_cons_headstroke.indexOf(txt.trim()) >= 0)
                    // if there's a vowel mark above, remove the headstroke from the consonant
                    telugu_kids.push(kid);
                else
                    kid.textContent = txt.replace(/\s+$/,'') + 'a';
            }
        }

        else if(kid.nodeType !== 1) continue;

        else if(txt === 'a') { 
            kid.textContent = '';
            continue;
        }
        
        else if(txt.trim().match(cons_regex)) {
            const last_txt = findTextNode(kid,true);
            last_txt.textContent = last_txt.textContent.replace(/\s+$/,'') + 'a';
        }

        if(!initial_vowels_allowed) {

            kid.classList.add('aalt');

            switch (script) {
                case 'devanagari':
                    if(txt === 'i' || txt === 'é') 
                        add_at_beginning.unshift(kid);
                    break;
                case 'grantha':
                case 'malayalam':
                    if(txt === 'e') add_at_beginning.unshift(kid);
                    else if(txt === 'ai') add_at_beginning.unshift(kid);
                    else if(txt === 'o') {
                        let new_e = kid.cloneNode(true);
                        replaceTextInNode('o','e',new_e);
                        add_at_beginning.unshift(new_e);
                        replaceTextInNode('o','ā',kid);
                    }
                    break;
                case 'telugu':
                    if(!telugu_del_headstroke &&
                       telugu_vowels.indexOf(txt) >= 0)
                        
                        telugu_del_headstroke = true;
                    break;

                }
        }
    } // end for let kid of kids

    for (let el of add_at_beginning) {
        node.insertBefore(el,node.childNodes[0]);
    }

    for (let el of telugu_kids) {
        el.textContent = el.textContent + 'a\u200D\u0C4D';
    }
}
*/

    const findTextNode = function(node,last = false) {
        if(node.nodeType === 3) return node;
        const walker = document.createTreeWalker(node,NodeFilter.SHOW_TEXT,null,false);
        if(!last) return walker.nextNode;
        else {
            let txt;
            while(walker.nextNode())
                txt = walker.currentNode;
            return txt;
        }
    };

    const detectTranspositions = function(lemma) {
        const middle = lemma.dataset.n;
        const par = lemma.parentElement;
        const clean_text = normalize(lemma.textContent,findNextLemma(lemma));
        const possibleTranspositions = [];
        backLoop:
        for(let m = middle-1;m>middle-5;m--) {
            const ownreading = par.querySelector('[data-n="'+m+'"]');
            if(ownreading &&
            normalize(ownreading.textContent,findNextLemma(ownreading)) === clean_text)
                continue;
            const candidates = document.querySelectorAll('[data-n="'+m+'"]');
            for(const candidate of candidates) {
                if(candidate.parentElement === par)
                    continue;
                if(normalize(candidate.textContent,findNextLemma(candidate)) === clean_text) {
                    possibleTranspositions.push(candidate);
                    break backLoop;
                }
            }
        }
        forwardLoop:
        for(let o = middle+1;o<middle+5;o++) {
            const ownreading = par.querySelector('[data-n="'+o+'"]');
            if(ownreading &&
            normalize(ownreading.textContent,findNextLemma(ownreading)) === clean_text)
                continue;
            const candidates = document.querySelectorAll('[data-n="'+o+'"]');
            for(const candidate of candidates) {
                if(candidate.parentElement === par)
                    continue;
                if(normalize(candidate.textContent,findNextLemma(candidate)) === clean_text) {
                    possibleTranspositions.push(candidate);
                    break forwardLoop;
                }
            }
        }
        if(possibleTranspositions.length > 0) {
            if(possibleTranspositions.length === 1)
                return possibleTranspositions[0].dataset.n;
            else
                return possibleTranspositions[0].dataset.n < possibleTranspositions[1].dataset.n ? possibleTranspositions[0].dataset.n : possibleTranspositions[1].dataset.n;
        }
        return false;
    };

    const findSelection = function() {    
        const sel = window.getSelection();
        if(sel.isCollapsed) return false;
    
        const range = (sel.rangeCount > 1) ? // firefox returns multiple ranges, chrome doesn't
            sel.getRangeAt(1).cloneContents() :
            sel.getRangeAt(0).cloneContents();
        if(!range) return false;

        const nums = new Set();
        const lemmata = range.querySelectorAll('.lemma');
        for(const lemma of lemmata) {
            nums.add(lemma.dataset.n);
        }
        return nums;
        /*
    const iterator = document.createNodeIterator(range.commonAncestorContainer,NodeFilter.SHOW_ALL,{acceptNode: function (node) {
            return NodeFilter.FILTER_ACCEPT;
            }
    });

    const nums = new Set();
    while (iterator.nextNode()) {
        const cur = iterator.referenceNode;
        if (nums.size === 0 && cur !== range.startContainer) continue;
        var node;
        if(cur.nodeType === 1)
            node = cur;
        else if(cur.nodeType === 3)
            node = cur.parentElement;
    
        if(node && node.classList.contains('lemma'))
                nums.add(node.dataset.n);
        else {
            const closest = node.closest('.lemma');
            if(closest)
                nums.add(closest.dataset.n);
        }

        if (cur === range.endContainer) break;
    }
    return nums;
*/
    };

    const makeXSLTProc = function(sheet) {
        var parser = new DOMParser();
        const xslsheet = parser.parseFromString(sheet,'text/xml');
        const xslt_proc = new XSLTProcessor();
        xslt_proc.importStylesheet(xslsheet);
        return xslt_proc;
    };

    const XSLTransformString = function(s,proc) {
        const temp = _xml.createElementNS(_teins,'ab');
        temp.innerHTML = s;
        //temp.setAttribute('xmlns','http://www.w3.org/1999/xhtml');
        return proc.transformToFragment(temp,document);
    };
    /*
const XSLTransformElement = function(el,proc) {
    const temp = _xml.createElementNS(_teins,'span');
    temp.appendChild(el.cloneNode(true));
    //temp.setAttribute('xmlns','http://www.w3.org/1999/xhtml');
    return proc.transformToFragment(temp,document);
}
*/
/*    const csvToFrag = function(csv) {
        const xslt_proc = makeXSLTProc(lemmaXSLT);
        const extras = ['|',',','-','―','?',' ','1','2','3','4','5','6','7','8','9','0'];
        const lemmaToFrag = function(s,n) {
            const newfrag = document.createDocumentFragment();
            const newspan = document.createElement('span');
            newspan.className = 'lemma';
            newspan.dataset.n = n;
            const trimmed = s.trim();

            if(s) {
                let addon = '';
                while(extras.indexOf(s.slice(-1)) > -1) {
                    addon = s.slice(-1) + addon;
                    s = s.slice(0,-1);
                }
                if(s === '') {
                    s = addon;
                    addon = '';
                }
                newspan.appendChild(XSLTransformString(s,xslt_proc));
                if(newspan.firstChild.nodeType === 1 &&
                newspan.firstChild.classList.contains('lg'))
                    newspan.classList.add('verse');
                touchUpNode(newspan);
                newfrag.appendChild(newspan);
                const addnode = document.createTextNode(touchUpText(addon + ' '));
                newfrag.appendChild(addnode);
            }
            else {
                newspan.classList.add('invisible');
                newspan.appendChild(document.createTextNode('\u00a0\u00a0\u00a0'));
                newfrag.appendChild(newspan);
                newfrag.appendChild(document.createTextNode(' '));
            }
            //newspan.IAST = newspan.cloneNode(true);
            return newfrag;
        };

        const els = csv.map(lemmaToFrag);
        const retfrag = document.createDocumentFragment();
        for(const el of els)
            retfrag.appendChild(el);
        return retfrag;
    };
*/
    /*
const reconstructText = function() {
    let fulltext = [];
    for(let el of _textdiv.getElementsByClassName('lemma')) {
        let paths = analyzeVariants(el.dataset.n);
        let reconstructed = reconstructLemma(paths);
        let finalreading;

        if(reconstructed.aliases.length === 0)
            finalreading = reconstructed.lemma;
        else {
            let lemma_witnesses = 0;
            for(let witness of Object.keys(_texts)) {
                if(_texts[witness][el.dataset.n] === reconstructed.lemma)
                    lemma_witnesses++;
            }
            let variants = {};
            for(let witness of reconstructed.aliases) {
                let reading = _texts[witness][el.dataset.n];
                if(variants.hasOwnProperty(reading))
                    variants[reading].push(witness);
                else
                    variants[reading] = [witness];
            }
            finalreading = reconstructed.lemma;
            let finalcount = lemma_witnesses;
            for(let variant of Object.keys(variants)) {
                if(variants[variant].length > finalcount) {
                    finalreading = variant;
                    finalcount = variant.length;
                }
            }
        }

        el.innerHTML = wrapIt(finalreading);
        fulltext.push(finalreading);
    }
    const file = new Blob([fulltext.toString()], {type: 'text/csv'});
    const fileURL = URL.createObjectURL(file);
    const anchor = document.createElement('a');
    anchor.href = fileURL;
    anchor.target = '_blank';
    anchor.download = 'reconstructed.csv';
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
}

const reconstructLemma = function(paths) {
    let cur_longest;
    let longest_lemma;
    let aliases = [];
    for(let lemma of Object.keys(paths)) {
        if(typeof paths[lemma] === 'string') {
            continue;
        }
        if(!cur_longest) {
            cur_longest = paths[lemma];
            longest_lemma = lemma;
        }
        else if(paths[lemma].length > cur_longest.length) {
            cur_longest = paths[lemma];
            longest_lemma = lemma;
        }
        else if(paths[lemma].length === cur_longest.length) {
            // same number of edges; use total branch 
            if(paths[lemma].branch_length > cur_longest.branch_length) {
                cur_longest = paths[lemma];
                longest_lemma = lemma;
            }
            else if(paths[lemma].branch_length === cur_longest.branch_length) {
                console.log('shit');
            }
        }
    }
    for(let key of Object.keys(paths)) {
        // keys are actually witness ids here
        if(typeof paths[key] === 'string' && paths[key] === longest_lemma)
            aliases.push(key);
    }
    return {lemma: longest_lemma, path: cur_longest, aliases: aliases}; 
}
*/

    /*** Same-window view-updating functions ***/

    const jiggle = function(node,script) {
        if(node.firstChild.nodeType !== 3 && node.lastChild.nodeType !== 3) 
            return;

        const kids = node.childNodes;
        //const vowels = ['ā','i','ī','u','ū','e','o','ṃ','ḥ','ai','au'];
        //    const vowels_regex = /[aāiīuūeoṛṝḷṃḥ_]$/;
        const starts_with_vowel = /^[aāiīuūeoṛṝḷṃḥ]/;
        const ends_with_consonant = /[kgṅcjñṭḍṇtdnpbmyrlvṣśsh]$/;

        const telugu_vowels = ['ā','i','ī','e','o','_','ai','au'];
        const telu_cons_headstroke = ['h','k','ś','y','g','gh','c','ch','jh','ṭh','ḍ','ḍh','t','th','d','dh','n','p','ph','bh','m','r','ḻ','v','ṣ','s'];
        var telugu_del_headstroke = false;
        var telugu_kids = [];
        //const initial_vowels_allowed = (kids[0].nodeType !== 3) ? true : false;
        var add_at_beginning = [];
        const starts_with_text = (kids[0].nodeType === 3);
        //    const ends_with_text = (kids[kids.length-1].nodeType === 3);

        for (let kid of kids) {
            if(kid.nodeType > 3) continue;

            const txt = kid.textContent.trim();
            if(txt === '') continue;
            if(txt === 'a') { 
                kid.textContent = '';
                continue;
            }

            if(txt.match(ends_with_consonant)) {
                // add 'a' if node ends in a consonant
                const last_txt = findTextNode(kid,true);
                last_txt.textContent = last_txt.textContent.replace(/\s+$/,'') + 'a';
                if(script === 'telugu' &&
               telu_cons_headstroke.indexOf(txt) >= 0) {
                // if there's a vowel mark in the substitution, 
                // remove the headstroke from any consonants
                    telugu_kids.push(kid);
                }
            }
        
            // case 1, use aalt:
            // ta<subst>d <del>ip</del><add>it</add>i</subst>
            // case 2, use aalt:
            // <subst>d <del>apy </del><add>ity </add>i</subst>va
            // case 3, no aalt:
            // <subst><del>apy </del><add>ity </add>i</subst>va
        
            // use aalt if node is a text node or 
            // if it starts with a vowel
            if(kid === node.lastChild && kid.nodeType === 3) {
                const cap = document.createElement('span');
                cap.appendChild(kid.cloneNode(false));
                node.replaceChild(cap,kid);
                kid = cap; // redefines 'kid'
                kid.classList.add('aalt');
            }

            else if(starts_with_text && txt.match(starts_with_vowel))
                kid.classList.add('aalt');
        
            switch (script) {
            case 'devanagari':
                if(txt === 'i' || txt === 'é') 
                    add_at_beginning.unshift(kid);
                break;
            case 'grantha':
            case 'malayalam':
                if(txt === 'e' || txt === 'ai') 
                    add_at_beginning.unshift(kid);
                else if(txt === 'o') {
                    const new_e = kid.cloneNode(true);
                    replaceTextInNode('o','e',new_e);
                    add_at_beginning.unshift(new_e);
                    replaceTextInNode('o','ā',kid);
                }
                break;
            case 'telugu':
                if(!telugu_del_headstroke &&
                   telugu_vowels.indexOf(txt) >= 0)
                    
                    telugu_del_headstroke = true;
                break;

            }
        } // end for let kid of kids

        for (const el of add_at_beginning) {
            node.insertBefore(el,node.firstChild);
        }

        if(telugu_del_headstroke) {
            for (const el of telugu_kids) {
                const lasttxtnode = findTextNode(el,true);
                lasttxtnode.textContent = lasttxtnode.textContent + '\u200D\u0C4D';
            }
        }
    };

    const replaceTextInNode = function(text, replace, node) {
        const walker = document.createTreeWalker(node,NodeFilter.SHOW_TEXT,null,false);
        while(walker.nextNode()) {
            let cur_txt = walker.currentNode.textContent;
            if(cur_txt.match(text))
                walker.currentNode.textContent = replace;
        }
    };

    const clearUnderlines = function() {
        const lemmata = document.querySelectorAll('.transposed');
        for(const lemma of lemmata)
            lemma.classList.remove('transposed');
    };

/*    const underlineVariants = function() {
        clearUnderlines();
        const transpositions = [];
        for(let n=0;n<_maxlemma;n++) {
            const lemma = document.querySelectorAll('.lemma[data-n="'+n+'"]:not(.invisible)');
            if(!lemma) continue;
            if(lemma.length === 1) {
                const new_n = detectTranspositions(lemma[0]);
                if(new_n) {
                    transpositions.push({el: lemma[0],new_n: new_n});
                }
            }
        }
        for(const trans of transpositions) {
            trans['el'].dataset.nTrans = trans['new_n'];
            trans['el'].classList.add('transposed');
        }
    };
*/
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
        //_menus = document.querySelectorAll('.menubox');
        const menu = document.getElementById('menu');
        //for(const menu of _menus) {
        menu.addEventListener('mouseover', menuMouseover);
        menu.addEventListener('mouseout', menuMouseout);
        menu.addEventListener('click',menuClick);
        // }
        menu.querySelector('#treefile').addEventListener('change',fileSelect.bind(null,treeFileLoad),false);
    };

    const newBox = {
        matrix: function() {
            _matrix = new MatrixBox();
            _matrix.init();
            _matrix.show();
            document.getElementById('matrixmenu').style.display = 'none';
            drawTrees();
            multi.rehighlight();
            return _matrix;
        },

        text: function(name,map) {
            const newEd = new EdBox(name);
            //const newEd = new EdBox(name,map);
            _textboxes.push(newEd);
            newEd.init();
            newEd.show();
            //underlineVariants();
            drawTrees();
            multi.rehighlight();
            if(!document.querySelector('.highlit'))
                textClick({target: newEd.boxdiv.querySelector('.lemma:not(.invisible)')});
            return newEd;
        },

        tree: function(stemmaid,id) {
            const newTree = new TreeBox(stemmaid,id);
            _trees.push(newTree);
            newTree.init();
            newTree.show();
            drawTrees();
            multi.rehighlight();
            return newTree;
        },
    };

    const drawTrees = function() {
        for(const tree of _trees) {
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

    const multiHighlight = function(nums) {
        if(!nums || nums.size === 0) return;
        multi.unHighlightAll();
    
        const [low,high] = find.lowhigh(nums);
        /*    
    const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
    const low = parseInt(sortednums[0]);
    const high = sortednums.length > 1 ?
        parseInt(sortednums[sortednums.length-1]) :
        undefined;
*/
        if(high !== undefined) {
            for(let n=low;n<=high;n++) multi.highlightLemma(n,true);
        }
        else
            multi.highlightLemma(low);
        multi.repopulateTrees(low,high);
        for(const box of _viewdiv.querySelectorAll('.text-box'))
            if(!box.querySelector('.highlit'))
                box.querySelector('[data-n="'+low+'"]').classList.add('highlit');      
        view.xScroll(low);
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
        return window['Hypher']['languages']['sa'].hyphenateText(
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
                _windows;
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
                multiHighlight(nums);
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
        _filename = f.name;
        const ext = _filename.split('.').pop();
        if(ext === 'csv') csvLoad(f,fs,e);
        else matrixLoad(f,fs,e);
    };

    const treeFileLoad = function(f,fs,e) {
        const treestr = e.target.result;
        const parser = new DOMParser();
        const nexml = parser.parseFromString(treestr,'text/xml');
        const xenoData = _xml.querySelector('teiHeader > xenoData') || (function() {
            const header = _xml.querySelector('teiHeader') || (function() {
                const h = _xml.createElementNS(_teins,'teiHeader');
                _xml.documentElement.appendChild(h);
                return h})();
            const newel = _xml.createElementNS(_teins,'xenoData');
            header.appendChild(newel);
            return newel;
        })();
        const stemmael = _xml.createElementNS(_teins,'stemma');
        stemmael.setAttribute('format','nexml');
        stemmael.id = 'stemma' + [...xenoData.querySelectorAll('stemma')].length;
        stemmael.appendChild(nexml.firstChild.cloneNode(true));
        xenoData.appendChild(stemmael);

        const otunodes = nexml.querySelectorAll('node[otu]');
        for(const otunode of otunodes) {
            const label = otunode.getAttribute('label');
            const otu = otunode.getAttribute('otu');
            if(label !== otu) otunode.setAttribute('label',otu);
        }

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
            const label = tree.getAttribute('label') || 'New Tree ' + _treelist.size;
            _treelist.set(`#${stemmaid} #${id}`,xclone);
            const li = document.createElement('li');
            li.dataset.name = label;
            li.dataset.treeid = id;
            li.dataset.stemmaid = stemmaid;
            li.appendChild(document.createTextNode(label));
            treemenu.insertBefore(li,treemenu.lastElementChild);

            if(show) newBox.tree(stemmaid,id);
        }
    };
/*
    const csvLoad = function(f,fs,e) {
        _filename = f.name;
        const ext = _filename.split('.').pop();
        var csvarr = [];
        if(ext === 'csv') {
            _xml = document.implementation.createDocument(_teins,'',null);
            const teicorpus = _xml.createElementNS(_teins,'teiCorpus');
            const teiheader = _xml.createElementNS(_teins,'teiHeader');
            teicorpus.appendChild(teiheader);
            _xml.appendChild(teicorpus);
            const csvstr = e.target.result;
            csvarr = csvstr.trim().split(/\n+/)
                .map(s => s.replace(/""/g,'').split(','))
                .map(a => {
                    const name = a.shift();
                    const tei = _xml.createElementNS(_teins,'TEI');
                    //tei.setAttribute('xmlns','http://www.tei-c.org/ns/1.0');
                    tei.setAttribute('n',name);
                    const text = _xml.createElementNS(_teins,'text');
                    //text.setAttribute('xmlns','http://www.tei-c.org/ns/1.0');
                    //const root = document.createElementNS('http://www.w3.org/1999/xhtml','text');
                    //root.setAttribute('n',name);
                    for(let n=0;n<a.length;n++) {
                        const word = a[n];
                        //const newEl = document.createElementNS('http://www.tei-c.org/ns/1.0','w');
                        //newEl.setAttribute('xmlns','http://www.tei-c.org/ns/1.0');
                        const newEl = _xml.createElementNS(_teins,'w');
                        //newEl.setAttribute('xmlns','http://www.w3.org/1999/xhtml');
                        if(word !== '')
                            newEl.appendChild(document.createTextNode(word));
                        newEl.setAttribute('n',n);
                        text.appendChild(newEl);
                    //root.appendChild(newEl);
                    }
                    tei.appendChild(text);
                    //_xml.appendChild(text);
                    teicorpus.appendChild(tei);
                    //_xml.appendChild(root);
                    return [name, {desc: name, text: a}];
                });
            _xml.normalize();
        }
        else if(ext === 'xml') {
            const xParser = new DOMParser();
            _xml = xParser.parseFromString(e.target.result,'text/xml');
            const teis = _xml.querySelectorAll('TEI');
            for(const tei of teis) {
                const name = tei.getAttribute('n');
                const words = tei.querySelectorAll('w');
                const wordarr = [...words].map(el => el.textContent);
                csvarr.push([name,{desc: name, text: wordarr}]);
            }
            const trees = _xml.querySelectorAll('teiHeader xenoData stemma nexml');
            for(const tree of trees) {
                const nexml = document.implementation.createDocument('http://www.nexml.org/2009','',null);
                nexml.appendChild(tree.cloneNode(true));
                treeXMLLoad(nexml,tree.closest('stemma').id,false);
            }
        }
        //_texts = new Map(csvarr);
        //_maxlemma = _texts.get(_texts.keys().next().value).text.length;
        //_maxlemma = find.firsttext().lastElementChild.getAttribute('n');
        _maxlemma = find.maxlemma();

        //var mss = Array.from(_texts.keys());
        //mss.sort();
        var msshtml = '';
        const mss = [...find.teis()].map(el => el.getAttribute('n'));
        for(const ms of mss)
        //    msshtml += `<li data-name="${ms}">${_texts.get(ms).desc}</li>`;
            msshtml += `<li data-name="${ms}">${ms}</li>`;
        document.getElementById('menu').querySelector('.ms').innerHTML = msshtml;
        const expbox = new menuBox('Export');
        expbox.populate([
            {text: 'TEI XML', func: exp.xml},
            {text: 'CSV', func: exp.showOptions.bind(null,exp.csv)},
            {text: 'NEXUS', func: exp.showOptions.bind(null,exp.nexus)}
        ]);
        const editbox = new menuBox('Edit');
        editbox.populate([
            {text: 'Undo', greyout: check.undo, func: edit.undo},
            {text: 'Redo', greyout: check.redo, func: edit.redo},
            {text: 'Delete',
                greyout: check.anyhighlit,
                func: edit.startRemoveCol.bind(null,false)
            },
            {text: 'Delete empty lemmata',
                func: edit.startRemoveEmptyewBox.matrix);
        matrixmenu.removeChild(matrixmenu.querySelector('ul'));
        document.getElementById('mssmenu').style.display = 'block';
        document.getElementById('treemenu').style.display = 'block';

    };
*
            },
            {text: 'Merge',
                greyout: check.manyhighlit,
                func: edit.startMerge.bind(null,false)
            },
            {text: 'Group',
                alt: 'Ungroup',
                greyout: check.oneGrouped,
                toggle: check.grouped,
                func: edit.startGroup.bind(null,false),
            },
            {text: 'Edit reading',
                greyout: check.highlitcell,
                func: edit.startEditCell.bind(null,false),
            },
            {text: 'Insert row',
                func: edit.startNewRow,
            },
            {text: 'Insignificant',
                checkbox: check.checkbox.bind(null,'insignificant',false),
                greyout: check.anyhighlit,
                func: edit.startMarkAs.bind(null,'insignificant',false)
            },
            {text: 'Binary',
                checkbox: check.checkbox.bind(null,'binary',false),
                greyout: check.anyhighlit,
                func: edit.startMarkAs.bind(null,'binary',false)
            },
        ]);

        const viewbox = new menuBox('View');
        viewbox.populate([
            {text: 'Header',
                checkbox: check.headerView,
                func: view.toggleHeader,
            },
            {text: 'Normalize',
                checkbox: check.normalizedView,
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
*/
    const csvLoad = function(f,fs,e) {
        const csvarr = CSV.parse(e.target.result,{delimiter: ","});
        _xml = document.implementation.createDocument(_teins,'',null);
        const teicorpus = _xml.createElementNS(_teins,'teiCorpus');
        const teiheader = _xml.createElementNS(_teins,'teiHeader');
        teicorpus.appendChild(teiheader);
        _xml.appendChild(teicorpus);

        for(const c of csvarr) {
            const name = c[0];
            const arr = c.slice(1);
            const tei = _xml.createElementNS(_teins,'TEI');
            tei.setAttribute('n',name);
            const text = _xml.createElementNS(_teins,'text');
            for(let n=0;n<arr.length;n++) {
                const word = arr[n];
                const newEl = _xml.createElementNS(_teins,'w');
                if(word)
                    newEl.appendChild(document.createTextNode(word));
                newEl.setAttribute('n',n);
                text.appendChild(newEl);
            }
            tei.appendChild(text);
            teicorpus.appendChild(tei);
        }
        _xml.normalize();

        _maxlemma = find.maxlemma();
        
//        if(fs.length > 0) csvLoadAdditional(fs);

        menuPopulate();
    }

    const matrixLoad = function(f,fs,e) {
        const xParser = new DOMParser();
        _xml = xParser.parseFromString(e.target.result,'text/xml');
        
        const trees = _xml.querySelectorAll('teiHeader xenoData stemma nexml');
        for(const tree of trees) {
            const nexml = document.implementation.createDocument('http://www.nexml.org/2009','',null);
            nexml.appendChild(tree.cloneNode(true));
            treeXMLLoad(nexml,tree.closest('stemma').id,false);
        }

        _maxlemma = find.maxlemma();
        
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
            for(const tei of find.teis()) {
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
            const lastlemma = _maxlemma + addlemma;
            for(const [key,val] of newteis) {
                const oldtext = oldteis.get(key).querySelector('text');
                const newtext = val.querySelector('text');
                const newwords = newtext.querySelectorAll('w[n]');
                
                var cur_n = _maxlemma + 1;
                for(const word of newwords) {
                    word.setAttribute('n',cur_n);
                    oldtext.appendChild(word);
                    cur_n = cur_n + 1;
                }
            }

            for(const el of tofill) {
                const oldtext = oldteis.get(el).querySelector('text');
                make.emptywords(text,lastlemma,_maxlemma + 1);
            }

            _maxlemma = find.maxlemma(); // can probably change this
            if(add.length > 0) matrixLoadAdditional(add);
            else menuPopulate();
        };

        const reader = new FileReader();
        reader.onload = go.bind(null,fss);
        reader.readAsText(f);
        
    };

    const menuPopulate = function() {
        var msshtml = '';
        const mss = [...find.teis()].map(el => el.getAttribute('n'));
        for(const ms of mss)
        //    msshtml += `<li data-name="${ms}">${_texts.get(ms).desc}</li>`;
            msshtml += `<li data-name="${ms}">${ms}</li>`;
        document.getElementById('menu').querySelector('.ms').innerHTML = msshtml;
        const expbox = new menuBox('Export');
        expbox.populate([
            {text: 'TEI XML', func: exp.xml},
            {text: 'CSV', func: exp.showOptions.bind(null,exp.csv)},
            {text: 'NEXUS', func: exp.showOptions.bind(null,exp.nexus)}
        ]);
        const editbox = new menuBox('Edit');
        editbox.populate([
            {text: 'Undo', greyout: check.undo, func: edit.undo},
            {text: 'Redo', greyout: check.redo, func: edit.redo},
            {text: 'Delete',
                greyout: check.anyhighlit,
                func: edit.startRemoveCol.bind(null,false)
            },
            {text: 'Delete empty lemmata',
                func: edit.startRemoveEmpty
            },
            {text: 'Merge',
                greyout: check.manyhighlit,
                func: edit.startMerge.bind(null,false)
            },
            {text: 'Group',
                alt: 'Ungroup',
                greyout: check.oneGrouped,
                toggle: check.grouped,
                func: edit.startGroup.bind(null,false),
            },
            {text: 'Edit reading',
                greyout: check.highlitcell,
                func: edit.startEditCell.bind(null,false),
            },
            {text: 'Insert row',
                func: edit.startNewRow,
            },
            {text: 'Insignificant',
                checkbox: check.checkbox.bind(null,'insignificant',false),
                greyout: check.anyhighlit,
                func: edit.startMarkAs.bind(null,'insignificant',false)
            },
            {text: 'Binary',
                checkbox: check.checkbox.bind(null,'binary',false),
                greyout: check.anyhighlit,
                func: edit.startMarkAs.bind(null,'binary',false)
            },
        ]);

        const viewbox = new menuBox('View');
        viewbox.populate([
            {text: 'Header',
                checkbox: check.headerView,
                func: view.toggleHeader,
            },
            {text: 'Normalize',
                checkbox: check.normalizedView,
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
        xml: function() {
            const xslt_proc = makeXSLTProc(prettyXSLT);
            const str = new XMLSerializer().serializeToString(
                //  XSLTransformElement(_xml,xslt_proc)
                xslt_proc.transformToDocument(_xml)
            );
            const file = new Blob([str], {type: 'text/xml;charset=utf-8'});
            const fileURL = find.basename() + '.xml';
            FileSaver(file,fileURL);
        },

        csv: function(doc) {
            const xslt_proc = makeXSLTProc(csvXSLT);
            //const str = new XMLSerializer().serializeToString(XSLTransformElement(doc,xslt_proc));
            //const str = new XMLSerializer().serializeToString(xslt_proc.transformToDocument(_xml));
            const str = xslt_proc.transformToDocument(_xml).documentElement.textContent;
            const file = new Blob([str], {type: 'text/csv;charset=utf-8'});
            const fileURL = find.basename() + '.csv';
            FileSaver(file,fileURL);
        },
  
        nexus: function(doc) {
            const texts = [...find.texts(doc)];
            const ntax = texts.length;
            const symbols = '0 1 2 3 4 5 6 7 8 9 A B C D E F G H K L M N P Q R S T U V W X Y Z a b c d e f g h k l m n p q r s t u v w x y z';
            const symbolarr = symbols.split(' ');
            const gap = '-';
            const taxlabels = texts.map(el => '\''+el.parentElement.getAttribute('n')+'\'');
            const textWalkers = texts.map(el => find.textWalker(el));
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
                    if(reading !== '')
                        statelabels.add(reading);
                }
                charstatelabels.push(statelabels);
                const statesymbols = new Map([...statelabels].map((x,i) => [x,symbolarr[i]]));
                for(let p=0;p<readings.length;p++) {
                    const r = readings[p] === '' ? gap : statesymbols.get(readings[p]);
                    matrix[p].push(r);
                }
            }
            const charstatestr = charstatelabels.map((x,i) => (i+1) +' / '+ [...x].map(s => `'${s}'`).join(' ')).join(',\n');
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
${matrix.map(arr => arr.join('')).reduce((acc,cur) => acc + '\n'+cur)}
;
END;
`;
            const file = new Blob([str], {type: 'text/nexus;charset=iso8859-1'});
            const fileURL = find.basename() + '.nex';
            FileSaver(file,fileURL);
        },

        processOptions: function(opts) {
            if(opts.get('option_normalize') && !check.anyNormalized()) 
                view.normalizeAll();

            const doc = _xml.cloneNode(true);

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
                        const nexttxt = el.firstChild.textContent;
                        if(nexttxt.trim() !== '')
                            firstw.textContent = firstw.textContent /*+ ' '*/ + nexttxt;
                        el.removeChild(el.firstChild);
                    }
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
                const els = doc.querySelectorAll('w[binary="true"]');
                const nonempty = [...els].filter(el => el.textContent !== '');
                const placeholder = '['+nonempty[0].textContent+']';
                for(const el of nonempty)
                    el.textContent = placeholder;

            }
            if(opts.get('option_noempty')) {
                const els = doc.querySelectorAll('w');
                for(const el of els)
                    if(el.textContent === '')
                        el.textContent = '[]';
            }
            return doc;
        },

        showOptions: function(func) {
            const blackout = document.createElement('div');
            blackout.id = 'blackout';
            const frag = document.createRange().createContextualFragment(
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
            blackout.appendChild(frag);
            document.body.appendChild(blackout);
            const submitfunction = function(e) {
                e.preventDefault();
                var opts = [];
                const inputs = document.getElementById('exportform').elements;
                for(const i of inputs) {
                    opts.push([i.name,i.checked]);
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
    const treeMouseover = function(e) {
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
        _viewdiv.parentElement.appendChild(box);

        const textbox = document.createElement('div');
        textbox.appendChild(document.createTextNode(title));
        this.script !== 0 ? // this is bound to the TreeBox 
            box.appendChild(changeScript(textbox,_scripts[this.script])) :
            box.appendChild(textbox);

        if(e.target.classList.contains('reconstructed')) {
            const treelemma = e.target.parentNode.querySelector('span.tree-lemma');
            if(treelemma && treelemma.dataset.hasOwnProperty('emended')) {
                const emendbox = document.createElement('div');
                emendbox.classList.add('emphasis');
                emendbox.appendChild(document.createTextNode(treelemma.textContent));
                this.script !== 0 ?
                    box.prepend(changeScript(emendbox,_scripts[this.script])) :
                    box.prepend(emendbox);
            }
        }

        window.getComputedStyle(box).opacity;
        box.style.opacity = 1;
    
        e.target.addEventListener('mouseout', removeBox);
    };

    const treeClick = function(e) {
        if(e.target.classList.contains('witness'))
            newBox.text(e.target.dataset.key);
        //newBox.text(e.target.dataset.key,_texts);
        else if(e.target.classList.contains('reconstructed'))
            newBox.text(e.target.dataset.label);
        //newBox.text(e.target.dataset.label,_texts);
        else if(e.target.classList.contains('internal'))
            edit.startReconstruction(e);
    
        removeBox();
    };

    const keyDown = function(e) {
        if(!_editing && e.key.substring(0,5) === 'Arrow') cycleVariant(e);
        else if(e.ctrlKey || e.metaKey)
            if(e.key === 'Z')
                edit.redo();
            else if(e.key === 'z')
                edit.undo();
    };

    const cycleVariant = function(e) {
        if(e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
            const cur = _viewdiv.querySelector('.highlit').dataset.n;
            if(e.key === 'ArrowRight') {
                let next = parseInt(cur)+1;
                while(next <= _maxlemma) {
                    const highlitcell = find.highlitcell();
                    if(highlitcell) {
                        textClick({target: highlitcell.nextElementSibling});
                        return;
                    }
                
                    const nextlemmata = _viewdiv.querySelectorAll('[data-n="'+next+'"]');
                    for(const nextlemma of nextlemmata) {
                        if(nextlemma && !nextlemma.classList.contains('invisible')) {
                            textClick({target: nextlemma});
                            return;
                        }
                    }
                    next++;
                }
            }
            
            if(e.key === 'ArrowLeft') {
                let prev = parseInt(cur) -1;
                while(prev >= 0) {
                    const highlitcell = find.highlitcell();
                    if(highlitcell) {
                        textClick({target: highlitcell.previousElementSibling});
                        return;
                    }

                    const prevlemmata = _viewdiv.querySelectorAll('[data-n="'+prev+'"]');
                    for(const prevlemma of prevlemmata) {
                        if(prevlemma && !prevlemma.classList.contains('invisible')) {
                            textClick({target: prevlemma});
                            return;
                        }
                    }
                    prev--;
                }
            }
        }
    };

    const textClick = function(e,skipRight = false) {
        if(e.target.closest('tr.header')) {
            matrixHeaderClick(e);
            return;
        }
        const targ = e.target.classList.contains('lemma') ? 
            e.target :
            e.target.closest('.lemma');

        if(targ) {
            if(!skipRight && e.ctrlKey) {
                rightClick(e);
                return;
            }
            const n = targ.dataset.n;
            const matrixrow = find.highlitrow();
            multi.unHighlightAll();
            multi.highlightLemma(n);
            multi.repopulateTrees(n);
            view.xScroll(n,matrixrow);
            if(targ.tagName === 'TD')
                targ.classList.add('highlitcell');

        }
    };

    const textMouseup = function() {
        const nums = findSelection();
        multiHighlight(nums);
        clearSelection();
    };

    /*const lemmaMouseover = function(e) {
    
    const title = e.target.dataset.title;
    if(!title) return;

    const box = document.createElement('div');
    box.id = 'tooltip';
    box.style.top = e.pageY + 'px';//(e.clientY + 10) + 'px';
    box.style.left = e.pageX + 'px';//e.clientX + 'px';
    box.style.opacity = 0;
    box.style.transition = 'opacity 0.2s ease-in';
    _viewdiv.parentElement.appendChild(box);

    const textbox = document.createElement('div');
    textbox.appendChild(document.createTextNode(title));
    box.appendChild(textbox);
    window.getComputedStyle(box).opacity;
    box.style.opacity = 1;
    
    e.target.addEventListener('mouseout', removeBox);
};
*/

    const menuMouseover = function(e) {
        const targ = e.target.classList.contains('menubox') ?
            e.target :
            e.target.closest('.menubox');
        if(targ) {
            const ul = targ.querySelector('ul');
            if(ul) ul.style.display = 'block';
            targ.classList.add('open');
        }
    };
    const menuMouseout = function(e) {
        const targ = e.target.classList.contains('menubox') ?
            e.target :
            e.target.closest('.menubox');
        if(targ) {
            const ul = targ.querySelector('ul');
            if(ul) ul.style.display = 'none';
            targ.classList.remove('open');
        }
    };
    const menuClick = function(e) {
        if(!e.target.parentElement) return;
        /*
    if(e.target.parentElement.className === 'ed') {
        menuMouseout(e);
        newBox.text(e.target.dataset.name,_editions);
    }
*/
        if(e.target.parentElement.className === 'ms') {
            menuMouseout(e);
            newBox.text(e.target.dataset.name);
        //newBox.text(e.target.dataset.name,_texts);
        }
        if(e.target.parentElement.className === 'tree') {
            menuMouseout(e);
            if(e.target.closest('li[data-name]'))
                newBox.tree(e.target.dataset.stemmaid,e.target.dataset.treeid);
        }
    };

    const thDragStart = function(e) {
        e.dataTransfer.setData('text/plain',e.target.textContent);
        //    _dragged.parentNode.classList.add('dragging');
        edit.startMoveRow(e.target.parentNode,e);
    };
    const trDragEnter = function(e) {
        const tr = e.target.nodeType === 1 ? 
            e.target.closest('tr') :
            e.target.parentElement.closest('tr');
        if(tr)
            tr.classList.add('dragenter');
    };
    const trDragLeave = function(e) {
        const tr = e.target.nodeType === 1 ?
            e.target.closest('tr') :
            e.target.parentElement.closest('tr'); 
        if(tr)
            tr.classList.remove('dragenter');
    };

    const trDragDrop = function(e) {
        e.preventDefault();
        /*    _dragged.parentNode.classList.remove('dragging');
    const tr = e.target.nodeType === 1 ?
            e.target.closest('tr') :
            e.target.parentElement.closest('tr');
    if(tr) {
        tr.classList.remove('dragenter');
        edit.doMoveRow(_dragged.parentNode,tr);
        _dragged = null;
    }
    */
        edit.finishMoveRow(e);
    };

    const matrixMousedown = function(e) {
        if(e.button !== 0) return;
        if(e.ctrlKey) {rightClick(e); return;}
        const lemma = e.target.nodeType === 1 ?
            e.target.closest('.lemma') :
            e.target.parentElement.closest('.lemma');
        if(lemma) {
        
            if(lemma.isContentEditable) return;

            multi.unHighlightAll();
            multi.highlightLemma(lemma.dataset.n);
            const tabl = _matrix.boxdiv.querySelector('table');
            tabl.addEventListener('mouseover',matrixMouseover);
            tabl.addEventListener('mouseup',matrixMouseup);
        }
    };

    const matrixMouseup = function(e) {
        const nums = find.highlit();
        multiHighlight(nums);
        const tabl = _matrix.boxdiv.querySelector('table');
        tabl.removeEventListener('mouseover',matrixMouseover);
        tabl.removeEventListener('mouseup',matrixMouseup);
    };

    const matrixMouseover = function(e) {
        const lemma = e.target.nodeType === 1 ?
            e.target.closest('.lemma') :
            e.target.parentElement.closest('.lemma');
        if(lemma)
            multi.highlightLemma(lemma.dataset.n);
    };

    const rightClick = function(e) {
        const th = e.target.nodeType === 1 ?
            e.target.closest('tr[data-n] th') :
            e.target.parentElement.closest('tr[data-n] th');
        if(th) {
            e.preventDefault();
            contextMenu.remove();
            const menu = contextMenu.create(e);
            const items = [
                {text: 'move',
                    func: edit.startMoveRow.bind(null,th.parentNode),
                },
                {
                    text: 'delete',
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
                (textClick(e,true), new Set([td.dataset.n])) :
                (function() {
                    const ret = find.highlit();
                    if(ret.size === 1 && 
                   !td.classList.contains('highlitcell')) {
                        multi.unCelllightAll(); 
                        td.classList.add('highlitcell');
                    }
                    return ret;
                })();
            const items = nums.size > 1 ? 
                [
                    {text: 'merge lemmata',
                        func: edit.startMerge.bind(null,nums)
                    },
                    {text: 'ungroup lemmata',
                        alt: 'group lemmata',
                        toggle: check.grouped,
                        func: edit.startGroup.bind(null,false)
                    },
                    {text: 'delete lemmata',
                        func: edit.startRemoveCol.bind(null,nums)
                    },
                    /*                {text: 'insignificant',
                 cond: check.checkbox.bind(null,'insignificant',nums),
                 func: edit.startMarkAs.bind(null,'insignificant',nums),
                },
                {text: 'binary',
                 cond: check.checkbox.bind(null,'binary',nums),
                 func: edit.startMarkAs.bind(null,'binary',nums),
                }, */
                ] : 
                [
                    {text: 'edit reading',
                        func: edit.startEditCell.bind(null,td)
                    },
                    {text: 'delete lemma',
                        func: edit.startRemoveCol.bind(null,nums)
                    },
                    /*                {text: 'insignificant',
                 cond: check.checkbox.bind(null,'insignificant',nums),
                 func: edit.startMarkAs.bind(null,'insignificant',nums),
                },
                {text: 'binary',
                 cond: check.checkbox.bind(null,'binary',nums),
                 func: edit.startMarkAs.bind(null,'binary',nums),
                }, */
                ];
            contextMenu.remove();
            const menu = contextMenu.create(e);
            contextMenu.populate(menu,items);
            contextMenu.show(menu);
        }
    };
    
    const matrixHeaderClick = function(e) {
        if(e.target.tagName !== 'INPUT') return;
        const type = e.target.className;
        if(type !== 'insignificant' && type !== 'binary') return;
        const num = e.target.closest('th').dataset.ref;
        const state = find.firsttd(num).dataset[type] === 'true' ? false : true;
        const states = new Map([[num,state]]);
        //if(find.firsttd(num).dataset[type] === 'true') e.target.checked = true;
        //else e.target.checked = false;
        //edit.startMarkAs(e.target.className,nums,e);
        edit.doMarkAs(type,states);
    };

    const edit = {
        undo: function() {
            const action = _undo.pop();
            if(action)
                action[0](...action[1]);
        },
        redo: function() {
            const action = _redo.pop();
            if(action)
                action[0](...action[1]);
        },
        doStack: function(entry,doing = 'do') {
            if(doing === 'undo') {
                entry[1].push('redo');
                _redo.push(entry);
            }
            else if(doing === 'redo') {
                entry[1].push('undo');
                _undo.push(entry);
            }
            else {
                entry[1].push('undo');
                _undo.push(entry);
                _redo = [];
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
            _dragged = targ;
            _dragged.classList.add('dragging');
            if(e.type !== 'dragstart') {
                for(const tr of find.trs())
                    tr.classList.add('moveinprogress');
                _matrix.boxdiv.querySelector('table').addEventListener('mousedown',edit.finishMoveRow);
            }
        },

        startMerge: function(nums,e) {
            const numss = nums === false ?
                find.highlit() :
                nums;
            const clgroups = find.clauses(numss,true);
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
                const toremove = find.clausesToRemove(clgroups,numss);
                if(!toremove)
                    edit.doMerge(numss,'do');
                else {
                    const args = toremove.map(s => [edit.doUngroup,[s]])
                        .concat([[edit.doMerge,[numss]]]);
                    edit.doMulti(args,'do');
                }

            }
        },

        startGroup: function(nums,e) {
            const numss = nums === false ?
                find.highlit() :
                nums;
            if(check.grouped())
                edit.startUngroup(numss);
            else
                edit.doGroup(numss,'do');
        },

        startUngroup: function(nums) {
            /*        const firstrow = _xml.querySelector('text');

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
            const clgroups = find.clauses(nums);
            const args = [...clgroups].map(s => [edit.doUngroup,[s]]);
            edit.doMulti(args,'do');
        },

        startRemoveCol: function(nums,e) {
            const numss = nums === false ?
                find.highlit() :
                nums;

            const clgroups = find.clauses(numss);
            if(!clgroups) {
                edit.doRemoveCol(numss,'do');
            }
            else {
                const toremove = find.clausesToRemove(clgroups,numss,1);
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
            const nums = find.empty();
            const clgroups = find.clauses(nums);
            if(!clgroups) {
                edit.doRemoveCol(nums,'do');
            }
            else {
                const toremove = find.clausesToRemove(clgroups,nums,1);
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
            const cell = el || _matrix.boxdiv.querySelector('td.highlitcell');
            //cell.classList.add('highlitcell');
            view.unnormalize(cell);
            cell.dataset.oldContent = cell.textContent;
        
            cell.contentEditable = 'true';
            cell.focus();
            _editing = true;
            const range = document.createRange();
            range.selectNodeContents(cell);
            const sel = window.getSelection();
            sel.removeAllRanges();
            sel.addRange(range);
            cell.addEventListener('blur',edit.finishEditCell);
            cell.addEventListener('keydown',edit.cellKeyDown);
        },

        cellKeyDown: function(e) {
            if(e.key === 'Enter')
                edit.finishEditCell(e);
        },
     
        startMarkAs: function(type,nums,e) {
            const targ = e.target.tagName === 'INPUT' ?
                e.target :
                e.target.querySelector('input');
            const numss = nums === false ?
                find.highlit() :
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
        _undo.push([edit.unmarkSignificance,[oldstates,true]]); */
        },
    
        startNewRow: function() {
            const tr = make.row('new row');
            const th = tr.querySelector('th');
            th.contentEditable = true;
            th.addEventListener('blur',edit.finishNewRow);
            th.addEventListener('keydown',edit.thKeyDown);

            _matrix.boxdiv.querySelector('tbody').appendChild(tr);
            th.scrollIntoView();
            th.focus();
            document.execCommand('selectAll',false,null);
            _editing = true;
        },

        startRenameRow: function(n) {
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

            _xml.documentElement.appendChild(tei);
        
            _editing = false;
            th.contentEditable = false;
            th.removeEventListener('blur',edit.finishNewRow);
            th.removeEventListener('keydown',edit.thKeyDown);
            // view.updateAllHeaders(); // new row is empty
            edit.doStack([edit.doDeleteRow,[label]],'do');
        },

        finishEditCell: function(e) {
            const cell = e.target;
            cell.classList.remove('highlitcell');
            _editing = false;
            cell.contentEditable = 'false';
            cell.removeEventListener('blur',edit.finishEditCell);
            cell.removeEventListener('keydown',edit.cellKeyDown);
            const content = cell.textContent;

            if(content === cell.dataset.oldContent) return;
        
            const cellnum = parseInt(cell.dataset.n);
            const row = cell.closest('tr');
            //const table = row.parentNode;
            const trs = [...find.trs()];
            const rownum = trs.indexOf(row);
            if(!cell.hasOwnProperty('IAST'))
                cell.IAST = cell.cloneNode(true);
            cell.IAST.textContent = content;
            edit.xmlChangeCell(cellnum,rownum,content);

            const tr = cell.closest('tr');
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

            view.renormalize(cellnum-1,cellnum+1,rownum);
            edit.refresh();
            //view.updateHeaders([cellnum]);
            view.updateAllHeaders(true);
        },

        finishMoveRow: function(e) {
            const tr = e.target.nodeType === 1 ?
                e.target.closest('tr') :
                e.target.parentElement.closest('tr');
            if(tr)
                edit.doMoveRow(_dragged,tr,'do');
            if(e.type !== 'drop') {
                for(const tr of find.trs())
                    tr.classList.remove('moveinprogress');
                _matrix.boxdiv.querySelector('table').removeEventListener('mousedown',edit.finishMoveRow);
            }
            else {
                for(const tr of find.trs())
                    tr.classList.remove('dragenter');
            }
            _dragged.classList.remove('dragging');
            _dragged = null;
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
        
            const tds = [...find.tds(false,tr)];
            const words = [...find.words(false,tei)];
        
            _matrix.boxdiv.querySelector('tbody').appendChild(tr);
            th.scrollIntoView();

            const workerblob = new Blob(['('+worker.fitch.toString()+')()'],{type: 'application/javascript'});
            const fitchWorker = new Worker(window.URL.createObjectURL(workerblob));
            const serialreadings = find.serializedtexts(tree.nexml);
            const seriallevels = find.serializedlevels(tree.levels);

            fitchWorker.postMessage({readings:serialreadings,levels:seriallevels,num:0,id:key});
            fitchWorker.onmessage = function(e) {
                const n = e.data.n;
                const reading = e.data.result;
                tds[n].textContent = reading;
                tds[n].IAST = tds[n].cloneNode(true);
                tds[n].classList.remove('pending');
                words[n].textContent = reading;
                if(n < _maxlemma)
                    fitchWorker.postMessage({readings:serialreadings,levels:seriallevels,num:n+1,id:key});
                else {
                    th.removeChild(spinner);
                    _xml.documentElement.appendChild(tei);
                    view.updateAllHeaders();
                    tree.draw();
                    const hl = find.highlit();
                    if(hl) multi.repopulateTrees(...find.lowhigh(hl));
                    edit.doStack([edit.doDeleteRow,[label]],'do');

                }
            };
        },

        doDeleteRow: function(label,doing = 'do') {
            const htmlrow = find.tr(label);
            const xmlrow = find.tei(label);
            const teis = [...find.teis()];
            const index = teis.indexOf(xmlrow);

            htmlrow.parentNode.removeChild(htmlrow);
            xmlrow.parentNode.removeChild(xmlrow);
            view.updateAllHeaders();
            drawTrees();
            edit.doStack([edit.doUndeleteRow,[htmlrow,xmlrow,index]],doing);
        },

        doUndeleteRow: function(htmlrow,xmlrow,index,doing = 'do') {
            const label = xmlrow.getAttribute('n');
            const teis = [...find.teis()];
            if(index === teis.length) {
                _xml.documentElement.appendChild(xmlrow);
                _matrix.boxdiv.querySelector('tbody').appendChild(htmlrow);
            }
            else {
                const trs = [...find.trs()];
                _xml.documentElement.insertBefore(xmlrow,teis[index]);
                trs[index].parentNode.insertBefore(htmlrow,trs[index]);
            }
            view.updateAllHeaders();
            drawTrees();
            edit.doStack([edit.doDeleteRow,[label]],doing);
        },

        doMoveRow: function(movetr,appendafter,doing = 'do') {
            const table = movetr.parentNode;
            //const trs = [...table.children];
            const trs = [...find.trs()];
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
                const root = _xml.documentElement;
                const teis = [...find.teis()];
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
                        view.unnormalize(cell);
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
                    const arr = [...nums].map(n => {
                        const cell = cellfunc(n,row);
                        view.unnormalize(cell);
                        return cell;
                    });
                    const arrclone = arr.map(el => el.cloneNode(true));
                    rowsclone.push(arrclone);
                    const reduced = arr.reduce(function(acc,cur) {
                        const targ = cur.IAST ? cur.IAST : cur;
                        if(targ.hasChildNodes()) {
                            /*if(acc.hasChildNodes())
                                acc.appendChild(document.createTextNode(' ')); */
                            while(targ.firstChild)
                                acc.appendChild(targ.firstChild);
                        }
                        cur.parentNode.removeChild(cur);
                        return acc;
                    });
                    reduced.normalize();
                    reduced.IAST = reduced.cloneNode(true);
                }
                return [rowfunc,cellfunc,rowsclone];
            };
            //const oldhtml = merge(document,'.matrix tr','td','data-n',nums);
            //const oldxml = merge(_xml,'text','w','n',nums);
            const oldhtml = merge(find.trs,find.firsttd,nums);
            const oldxml = merge(find.texts,find.firstword,nums);
            const start = parseInt([...nums][0]);
            edit.renumber(start);
            //edit.renumber(document,'.matrix tr','td','data-n',start);
            //edit.renumber(_xml,'text','w','n',start);
            edit.restyleGroups(nums);
            view.renormalize(start,start+1);
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
                const attr = find.whichattr(oldels[0][0]);
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
            const end = parseInt(sortednums[sortednums.length-1])+1;
            edit.renumber(start);
            edit.reIAST(nums);
            view.renormalize(start,end);
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
        
            const texts = find.texts();
            for(const text of texts) {
                const cl = document.createElementNS('http://www.w3.org/1999/xhtml','cl');
                const firstw = find.firstword(firstnum,text);
                //const firstw = text.querySelector('w[n="'+firstnum+'"]');
                firstw.parentNode.insertBefore(cl,firstw);
                cl.appendChild(firstw);
                for(const num of nums)
                    cl.appendChild(find.firstword(num,text));
                //cl.appendChild(text.querySelector('w[n="'+num+'"]'));
            }

            const lastnum = numarr.pop();
        
            for(const td of find.tds(firstnum)) {
                td.classList.add('group-start');
            }
            for(const td of find.tds(lastnum)) {
                td.classList.add('group-end');
            }
            for(const num of numarr) {
                for(const td of find.tds(num)) {
                    td.classList.add('group-internal');
                }
            }
            if(doing === 'multido')
                return [edit.doUngroup,[nums]];
            else edit.doStack([edit.doUngroup,[nums]],doing);
        },
   
        doUngroup: function(nums,doing = 'do') {
        //const texts = _xml.querySelectorAll('text');
            const texts = find.texts();

            // ungroup xml
            for(const text of texts) {
                let cl;
                for(const num of nums) {
                    const word = find.firstword(num,text);
                    //const word = text.querySelector('w[n="'+num+'"]');
                    if(!cl) cl = word.closest('cl');
                    cl.parentNode.insertBefore(word,cl);
                }
                cl.parentNode.removeChild(cl);
            }
        
            // ungroup html
            const tds = [...nums].map(
                n => [...find.tds(n)]
            ).reduce((a,b) => a.concat(b),[]);
            for(const td of tds) {
                td.classList.remove('group-start');
                td.classList.remove('group-internal');
                td.classList.remove('group-end');
            }
        
            if(doing === 'multido')
                return [edit.doGroup,[nums]];
            else
                edit.doStack([edit.doGroup,[nums]],doing);
        },

        doRemoveCol: function(nums,doing = 'do') {
            const remove = function(rowfunc,cellfunc,nums) {
                const rows = rowfunc();
                var rowsclone = [];
                for(const row of rows) {
                    const arr = [...nums].map(n => {
                        const cell = cellfunc(n,row);
                        view.unnormalize(cell);
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
            //const oldxml = remove(_xml,'text','w','n',nums);
            const oldhtml = remove(find.trs,find.firsttd,nums);
            const oldxml = remove(find.texts,find.firstword,nums);
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const start = parseInt([...sortednums][0])-1;
            edit.renumber(start);
            //edit.renumber(document,'.matrix tr','td','data-n',start);
            //edit.renumber(_xml,'text','w','n',start);
            view.renormalize(start,start+1);
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
                const attr = find.whichattr(oldels[0][0]);
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
            const end = parseInt(sortednums[sortednums.length-1])+1;
            edit.renumber(start);
            const highlitcells = document.querySelectorAll('.highlitcell');
            for(const cell of highlitcells)
                cell.classList.remove('highlitcell');
            edit.reIAST(nums);
            view.renormalize(start,end);
            edit.refresh();
            edit.restyleGroups(nums,true);
            view.updateAllHeaders();

            if(doing === 'multido')
                return [edit.doRemoveCol,[nums]];
            else
                edit.doStack([edit.doRemoveCol,[nums]],doing);
        },

        doEmend: function(cellnum,rownum,doing = 'do') {
            const tr = [...find.trs()][rownum];
            const td = find.firsttd(cellnum,tr);
            td.dataset.emended = true;

            const text = [...find.texts()][rownum];
            const word = find.firstword(cellnum,text);
            word.setAttribute('emended','true');
            if(doing === 'multido')
                return [edit.doUnemend,[cellnum,rownum]];
            else
                edit.doStack([edit.doUnemend,[cellnum,rownum]],doing);

        },

        doUnemend: function(cellnum,rownum,doing = 'do') {
            const tr = [...find.trs()][rownum];
            const td = find.firsttd(cellnum,tr);
            delete td.dataset.emended;

            const text = [...find.texts()][rownum];
            const word = find.firstword(cellnum,text);
            word.removeAttribute('emended');
            if(doing === 'multido')
                return [edit.doEmend,[cellnum,rownum]];
            else
                edit.doStack([edit.doEmend,[cellnum,rownum]],doing);
        },

        doChangeCell: function(cellnum,rownum,content,doing = 'do') {
            const oldcontent = edit.xmlChangeCell(cellnum,rownum,content);
            edit.htmlChangeCell(cellnum,rownum,content);
            view.renormalize(cellnum-1,cellnum+1,rownum);    
            edit.refresh();
            view.updateAllHeaders(true);

            if(doing === 'multido')
                return [edit.doChangeCell,[cellnum,rownum,oldcontent]];
            else
                edit.doStack([edit.doChangeCell,[cellnum,rownum,oldcontent]],doing);
        },

        htmlChangeCell: function(cellnum,rownum,content) {
            const row = [...find.trs()][rownum];
            const cell = find.firsttd(cellnum,row);
            //const row = document.querySelector('.matrix table')
            //                    .querySelectorAll('tr')[rownum];
            //const cell = row.querySelector('td[data-n="'+cellnum+'"]');
            view.unnormalize(cell);
            const oldcontent = cell.textContent;
            cell.textContent = content;
            if(cell.IAST) cell.IAST = cell.cloneNode(true);
            return oldcontent;
        },
    
        xmlChangeCell: function(cellnum,rownum,content) {
            const row = [...find.texts()][rownum];
            const cell = find.firstword(cellnum,row);
            //const row = _xml.querySelectorAll('text')[rownum];
            //const cell = row.querySelector('w[n="'+cellnum+'"]');
            view.unnormalize(cell);
            const oldcontent = cell.textContent;
            if(cell.childNodes.length === 0)
                cell.appendChild(document.createTextNode(content));
            else
                cell.textContent = content;
            return oldcontent;
        },

        doMarkAs: function(type,states,doing = 'do') {
            const nums = [...states.keys()];
            const oldstates = find.attr(type,nums);
            for(const num of nums) {
                const cells = find.tds(num);
                const words = find.words(num);
                //const cells = document.querySelectorAll('.matrix table td[data-n="'+num+'"]');
                //const words = _xml.querySelectorAll('w[n="'+num+'"]');
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
                const checkbox = find.checkbox(num,type);
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
            const par = _xml.querySelector('[n="'+key+'"] text');
            const text = [...par.querySelectorAll('w')].map(w => w.innerHTML);
            newcsvarr.push([key,{desc: value.desc, text: text}]);
        }
        _texts = new Map(newcsvarr);
        for(const box of _textboxes)
            box.refresh();
*/
            for(const textbox of _textboxes)
                textbox.refresh();
            multi.rehighlight();
            if(!check.anyhighlit())
                multi.clearTrees();
            else
                multi.repopulateTrees(...find.lowhigh(find.highlit()));
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
                    const attr = find.whichattr(els[0]);
                    for(var n=parseInt(start)+1;n < els.length;n++)
                        els[n].setAttribute(attr,n);
                }
            };
            dorenumber(find.trs,find.tds,start);
            dorenumber(x => [true],find.ths,start);
            dorenumber(find.texts,find.words,start);
            _maxlemma = find.maxlemma();
        //_maxlemma = find.firsttext().lastElementChild.getAttribute('n');
        },
    
        reIAST: function(nums) {
            const lemmata = [...nums].map(n => [...find.lemmata(n)]).flat();
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
                const word = find.firstword(num);
                const tds = find.tds(num);
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
    };

    const view = {
        toggleNormalize: function() {
            if(check.normalizedView())
                view.showUnnormalized();
            else
                view.showNormalized();
        },
        showNormalized: function(box) {
            const par = box ? box : document.getElementById('views');
            if(!box) par.classList.add('normalized');
            //const par = box ? box : _matrix.boxdiv;

            if(!check.anyNormalized()) {
                view.normalizeAll();
                for(const textbox of _textboxes)
                    textbox.refresh();
                multi.rehighlight();
            }
            else {
                for(const textbox of _textboxes)
                    textbox.updatescript();
            }
            _matrix.updatescript();
            if(!check.anyhighlit())
                multi.clearTrees();
            else
                multi.repopulateTrees(...find.lowhigh(find.highlit()));
            //const tds = matrix.querySelectorAll('td[data-normal]');
            /*        const lemmata = find.normal(par);
        for(const lemma of lemmata) {
            lemma.textContent = lemma.dataset.normal;
            // do script change stuff here
        }
*/        
            view.updateAllHeaders(true);
            view.xScrollToHighlit();
        },
        showUnnormalized: function() {
            document.getElementById('views').classList.remove('normalized');
            //const tds = _matrix.boxdiv.querySelectorAll('td[data-normal]');
            //const lemmata = find.normal();
            /*        for(const lemma of lemmata) {
            lemma.textContent = lemma.IAST.textContent;
            // do script change stuff here
        }*/
            _matrix.updatescript();
            for(const textbox of _textboxes)
                textbox.updatescript();
            /*        for(const tree of _trees) {
            tree.fitch();
            tree.updatescript();
        } */
            if(!check.anyhighlit())
                multi.clearTrees();
            else
                multi.repopulateTrees(...find.lowhigh(find.highlit()));
            view.updateAllHeaders(true);
            view.xScrollToHighlit();
        },
    
        normalizeAll: function() {
            return false;
            const htmlrows = [...find.trs()];
            const xmlrows = [...find.texts()];
            for(let n=0;n<xmlrows.length;n++) {
                const tr = htmlrows[n];
                const text = xmlrows[n];
                const words = [...find.words(false,text)];
                //            for(const td of tds)
                //                view.normalize(td,tr);
                let i = 0;
                let word = words[i];
                while(word) {
                    i = view.normalize(i,words);
                    const lemma = word.getAttribute('lemma');
                    if(lemma) {
                        const td = find.firsttd(word.getAttribute('n'),tr);
                        td.dataset.normal = lemma;
                    }
                    word = words[i];
                }
            }
            //_normalization = true;
            //const normalized = _matrix.boxdiv.querySelectorAll('td[data-normal]');
            /*        const normalized = find.normal();
        for(const td of normalized) {
            const word_n = td.dataset.n;
            const row_n = htmlrows.indexOf(td.closest('tr'));
            const word = find.firstword(word_n,xmlrows[row_n]);
            //const word = xmlrows[row_n].querySelector('w[n="'+word_n+'"]');
            word.setAttribute('lemma',td.dataset.normal);
        }
        */
        },
    
        renormalize: function(startnum, endnum, rownum=false) {
            return false;
            if(!check.anyNormalized()) return false;

            const normalized = check.normalizedView();
            const htmlrows = [...find.trs()];
            const xmlrows = [...find.texts()];
            const rownums = rownum ? [rownum] : [...htmlrows.keys()];
            const doNormal = function(index,words,htmlrow) {
                const word = words[index];
                const n = word.getAttribute('n');
                const td = find.firsttd(n,htmlrow);
                if(td.dataset.hasOwnProperty('normal'))
                    delete td.dataset.normal;
                if(word.hasAttribute('lemma'))
                    word.removeAttribute('lemma');

                const nextindex = view.normalize(index,words);
                const lemma = word.getAttribute('lemma');

                if(lemma) {
                    td.dataset.normal = lemma;
                    if(normalized)
                        td.textContent = td.dataset.normal;
                }
                else
                    td.textContent = td.IAST.textContent;

                return nextindex;
            };

            for(const r of rownums) {
                const htmlrow = htmlrows[r];
                const xmlrow = xmlrows[r];
                const words = [...find.words(false,xmlrow)];
                const firstword = find.firstword(startnum,xmlrow);
                const startn = words.indexOf(firstword);

                //let num = find.prevNonempty(startn-1,tds);
                //if(!num) num = startn;
                //let nextdatasetn;

                let cur = find.prevNonempty(startn-1,words);
                if(!cur) cur = startn;
                let curword = words[cur];
                while(curword.getAttribute('n') <= endnum) {
                    cur = doNormal(cur,words,htmlrow);
                    curword = words[cur];
                }
            
                const lastindex = doNormal(cur,words,htmlrow);
                const lastword = lastindex ? words[lastindex] : false;
                if(lastword && lastword.getAttribute('prenormal'))
                    lastword.removeAttribute('prenormal');
                //num = nexttd ? nexttd.dataset.n : num + 1;
                /*            do {
                //const td = find.firsttd(num,htmlrow);
                //const word = find.firstword(num,xmlrow);
                const td = tds[num];
                const datasetn = td.dataset.n;
                const word = find.firstword(datasetn,xmlrow);
                if(td.dataset.hasOwnProperty('normal'))
                    delete td.dataset.normal;
                if(word.hasAttribute('lemma'))
                    word.removeAttribute('lemma');

                //const nexttd = view.normalize(num,row);
                num = view.normalize(num,tds);
                nextdatasetn = num ? tds[num].dataset.n : false;

                if(td.dataset.hasOwnProperty('normal')) {
                    word.setAttribute('lemma',td.dataset.normal);
                    if(normalized)
                        td.textContent = td.dataset.normal;
                }
                else
                    td.textContent = td.IAST.textContent;
                //num = nexttd ? nexttd.dataset.n : num + 1;
                
            } while(nextdatasetn && nextdatasetn <= endnum);
            
            const lasttd = nextdatasetn ? tds[num] : false;
            if(nextdatasetn > endnum) {
                if(lasttd.dataset.hasOwnProperty('normal'))
                    delete lasttd.dataset.normal;
                const postlastn = view.normalize(num,tds);
                const postlasttd = postlastn ? tds[postlastn] : false;
                if(lasttd.dataset.hasOwnProperty('normal')) {
                    if(normalized)
                        lasttd.textContent = lasttd.dataset.normal;
                    const lastword = find.firstword(nextdatasetn,xmlrow);
                    lastword.setAttribute('lemma',lasttd.dataset.normal);

                }
                else lasttd.textContent = lasttd.IAST.textContent;
                if(postlasttd && postlasttd.dataset.hasOwnProperty('prenormal'))
                    delete postlasttd.dataset.prenormal;
            }
            else {
                if(lasttd && lasttd.dataset.hasOwnProperty('prenormal'))
                    delete lasttd.dataset.normal;
            } */
            }
        },

        normalize: function(index,words) {
            /*        const nextTD = function(td) {
            var el = td.nextElementSibling;
            while(el) {
                if(el.textContent !== '') return el;
                el = el.nextElementSibling;
            }
            return false;
        }*/

            //const arr = tds ? tds : find.tds(false,td.closest('tr'));
            //const td = arr[index];
            const word = words[index];
            /*
        if(!td.hasOwnProperty('IAST')) {
            td.IAST = td.cloneNode(true);
        }
        const txt = td.IAST.textContent;
        */
            const txt = word.textContent;
            const nextindex = find.nextNonempty(index+1,words);

            if(txt !== '' && txt[0] !== '{') {
            //const nexttd = nextTD(td);
                const nextword = nextindex ? words[nextindex] : false;
                const nextstr = nextword ? nextword.textContent : '';
                let normal,nextnormal;
                const prenormal = word.getAttribute('prenormal');
                if(prenormal) {
                    [normal,nextnormal] = normalize(prenormal,nextstr,true);
                    word.removeAttribute('prenormal');
                }
                else
                    [normal,nextnormal] = normalize(txt.trim(),nextstr,true);

                if(normal !== txt.trim())
                    word.setAttribute('lemma',normal);
                if(nextnormal !== nextstr.trim())
                    nextword.setAttribute('prenormal',nextnormal);
            }
            else { // for {{}} fields that get prenormalized 
                if(word.getAttribute('prenormal'))
                    word.removeAttribute('prenormal');
            }
            return nextindex;
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

        toggleHeader: function() {
            const header = _matrix.boxdiv.querySelector('tr.header');
            if(header.style.display === 'none')
                header.style.display = 'table-row';
            else
                header.style.display = 'none';
        },

        updateHeaders(nums) {
            for(const num of nums) {
                const th = find.firstth(num);
                const [count,unique] = find.readings(num);
                const readspan = th.querySelector('span.readings');
                const readings = count < 2 ? count : `${count}(${unique.size})`;
                readspan.textContent = readings;
            }
        },
    
        updateAllHeaders(readingsonly = false) {
            const trs = [...find.trs()];
            const trwalkers = trs.map(tr => find.trWalker(tr));
            const tds = [...find.tds(false,trs[0])];
            const ths = [...find.ths()];
            const head = _matrix.boxdiv.querySelector('tr.header');
            const newTh = function() {
                const th = document.createElement('th');
                const span = document.createElement('span');
                span.classList.add('readings');
                const form = document.createElement('form');
                form.innerHTML = '<div><input class="insignificant" type="checkbox">Insignificant</div><div><input class="binary" type="checkbox">Binary</div>';
                th.appendChild(span);
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
            const hl = find.highlit();
            if(hl) view.xScroll([...hl][0]);
        },    
        xScroll: function(num,row) {
            if(!num) return;
            const par = row || find.firsttr();
            const el = find.firsttd(num,par);
            const elrect = el.getBoundingClientRect();
            const matrix = _matrix.boxdiv;
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

    const find = {
        basename: function() {
            return _filename.split(/\.[^.]+$/)[0];
        },
        range: function(a,b) {
            return Array.from(Array(parseInt(b)-parseInt(a)+1).keys(), x => x+a);
        },
        maxlemma: function() {
            return [...find.firsttext().querySelectorAll('w[n]')].length-1;
        },
        lemmata: function(num,par) {
            const el = par ? par : document.querySelector('#views');
            return num === false ?
                el.querySelectorAll('.lemma') :
                el.querySelectorAll(`.lemma[data-n="${num}"]`);
        },

        tds: function(num,row) {
            const el = row ? row : _matrix.boxdiv;
            if(num === false)
                return el.querySelectorAll('td[data-n]');
            else
                return el.querySelectorAll(`td[data-n="${num}"]`);
        },

        firsttd: function(num,row) {
            const el = row ? row : _matrix.boxdiv;
            return el.querySelector(`td[data-n="${num}"]`);
        },

        tr: function(label) {
            return _matrix.boxdiv.querySelector(`tr[data-n="${label}"]`);
        },

        trs: function(element) {
            const el = element ? element : _matrix.boxdiv;
            return el.querySelectorAll('tr[data-n]');
        },

        firsttr: function(element) {
            const el = element ? element : _matrix.boxdiv;
            return el.querySelector('tr[data-n]');
        },
    
        trWalker: function(tr) {
            return document.createNodeIterator(tr,NodeFilter.SHOW_ELEMENT,
                {acceptNode: function(node) {if(node.tagName === 'TD') return NodeFilter.FILTER_ACCEPT;}},
                false);
        },

        textWalker: function(text) {
            return document.createNodeIterator(text,NodeFilter.SHOW_ELEMENT,
                {acceptNode: function(node) {if(node.tagName.toLowerCase() === 'w') return NodeFilter.FILTER_ACCEPT;}},
                false);
        },

        tei: function(label) {
            return _xml.querySelector(`TEI[n="${label}"]`);
        },

        teis: function() {
            return _xml.querySelectorAll('TEI');
        },

        texts: function(element) {
            const el = element ? element : _xml;
            return el.querySelectorAll('text');
        },
    
        serializedtexts: function(tree) {
            const otus = [...tree.querySelectorAll('otu[label]')].map(el => el.getAttribute('label'));
            const teis = [...find.teis()].filter(el => otus.indexOf(el.getAttribute('n')) !== -1);
            return new Map(
                teis.map(t => [
                    tree.querySelector(`node[label="${t.getAttribute('n')}"]`).getAttribute('id'),
                    [...t.querySelectorAll('w')].map(w => {
                        return check.normalizedView() && w.hasAttribute('lemma') ?
                            w.getAttribute('lemma') :
                            w.textContent;
                    })
                ])
            );
        },
    
        serializedlevels: function(levels) {
            return levels.map(arr => {
                if(arr instanceof Map) {
                    const newmap = new Map();
                    for(const [key,value] of arr)  {
                        newmap.set(key.getAttribute('id'),value.map(node => node.getAttribute('id')));
                    }
                    return newmap;
                }
                else {
                    return arr.map(node => node.getAttribute('id'));
                }
            });
        },

        firsttext: function(id) {
            return !id ? 
                _xml.querySelector('text') :
                _xml.querySelector(`[n="${id}"] text`);
        },

        words: function(num,text) {
            const el = text ? text : _xml;
            if(num === false)
                return el.querySelectorAll('w[n]');
            else
                return el.querySelectorAll(`w[n="${num}"]`);
        },

        firstword: function(num,row) {
            const el = row ? row : _xml;
            return el.querySelector(`w[n="${num}"]`);
        },

        normal: function(el) {
            const par = el ? el : document.getElementById('views');
            //const par = el ? el : _matrix.boxdiv;
            return par.querySelectorAll('.lemma[data-normal], .tree-lemma[data-normal]');
        },
    
        htmlreading: function(el) {
            return check.normalizedView() && el.dataset.normal ?
                el.dataset.normal :
                el.IAST.textContent;
        },
    
        xmlreading: function(label,n) {
            const el = _xml.querySelector(`TEI[n="${label}"] > text > w[n="${n}"]`);
            return check.normalizedView() && el.hasAttribute('lemma') ?
                el.getAttribute('lemma') :
                el.textContent;
        },
    
        xmlreadings: function(label) {
            const els = [..._xml.querySelectorAll(`TEI[n="${label}"] > text > w`)];

            return els.map(el => {
                check.normalizedView() && el.hasAttribute('lemma') ?
                    el.getAttribute('lemma') :
                    el.textContent;
            });
        },

        ths: function() {
            return _matrix.boxdiv.querySelectorAll('th[data-ref]');
        },

        firstth: function(num) {
            return _matrix.boxdiv.querySelector(`th[data-ref="${num}"]`);
        },
        checkbox: function(num,type) {
            return _matrix.boxdiv.querySelector(`th[data-ref="${num}"] input.${type}`);
        },

        highlit: function() {
            const firstrow = find.firsttr();
            const lemmata = firstrow.querySelectorAll('.highlit');
            if(lemmata.length === 0) return false;
            const nums = new Set();
            for(const lemma of lemmata) {
                nums.add(lemma.dataset.n);
            }
            return nums;
        },

        highlitcell: function() {
            return _matrix.boxdiv.querySelector('td.highlitcell');
        },

        highlitrow: function() {
            const highlitcell = find.highlitcell();
            return highlitcell ? highlitcell.closest('tr') : false;
        },

        lowhigh: function(nums) {
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const low = parseInt(sortednums[0]);
            const high = sortednums.length > 1 ?
                parseInt(sortednums[sortednums.length-1]) :
                undefined;
            return [low,high];
        },

        readings: function(num, element) {
            const el = element ? element : _matrix.boxdiv;
            const tds = find.tds(num,el);
            var count = 0;
            const unique = new Set();
            for(const td of tds) {
                const txt = td.textContent;
                if(txt !== '') {
                    count++;
                    unique.add(txt);
                }
            }
            return [count,unique.size];
        },

        attr: function(type,nums) {
            const firstrow = find.firsttr();
            var states = [];
            for(const num of nums) {
                const cell = find.firsttd(num,firstrow);
                if(cell.dataset[type] === 'true')
                    states.push([num,true]);
                else
                    states.push([num,false]);
            }
            return new Map(states);
        },
    
        whichattr: function(el) {
            if(el.hasAttribute('n')) return 'n';
            else if(el.hasAttribute('data-n')) return 'data-n';
            else if(el.hasAttribute('data-ref')) return 'data-ref';
            else return false;
        },

        clauses: function(nums,strict = false) {
            const firstrow = find.firsttext();
            var someungrouped = false;

            // make a list of clauses
            const cls = new Set();
            for(const num of nums) {
                const word = find.firstword(num,firstrow);
                const cl = word.closest('cl');
                if(cl) cls.add(cl);
                else if(strict) someungrouped = true;
            }
        
            if(cls.size === 0) return false;
            else {
            // get list of numbers in each clause
                const clgroups = [...cls].map(cl => {
                    const words = cl.querySelectorAll('w');
                    return new Set([...words].map(w => w.getAttribute('n')));
                });
                if(someungrouped) {
                    return [null].concat(clgroups);
                }
                else
                    return clgroups;
            }
        },
    
        clausesToRemove: function(clgroups,nums,threshold = 0) {
            const toremove = new Set();
            for(const group of clgroups) {
                const clone = new Set(group);
                for(const num of nums)
                    clone.delete(num);
                if(clone.size <= threshold)
                    toremove.add(group);
            }
            return [...toremove];
        },
    
        empty: function() {
            const emptyset = new Set();
            const trs = [...find.trs()];
            const trWalkers = trs.map(el => find.trWalker(el));
            const max = trs[0].querySelector('td:last-of-type').dataset.n;
            for(let n=0;n<=max;n++) {
                var emptylemma = true;
                for(const walker of trWalkers) {
                    const word = walker.nextNode();
                    if(emptylemma) {
                        if(word.textContent !== '')
                            emptylemma = false;
                    }
                }
                if(emptylemma) emptyset.add(n);
            }
            return emptyset;
        },

        prevNonempty: function(index,arr) {
            for(let n=index;n>=0;n--) {
                const td = arr[n];
                if(td.textContent !== '') return n;
            }
            return false;
            /*         var n = num;
        var td = find.firsttd(n,row);
        while(td) {
            if(td.textContent !== '') return n;
            n--;
            td = find.firsttd(n,row);
        }
        return num; */
        },
    
        nextNonempty: function(index,arr) {
            for(let n=index;n<arr.length;n++) {
                const td = arr[n];
                if(td.textContent !== '') return n;
            }
            return false;
        },

        setIntersection: function(...sets) {
            const setA = sets[0];
            return new Set(
                [...setA].filter(el => {
                    for(let n=1;n<sets.length;n++) {
                        if(!sets[n].has(el))
                            return false;
                    }
                    return true;
                })
            );
        },

        setUnion: function(...sets) {
            return new Set(
                sets.reduce((acc, cur) => {
                    acc = [...acc,...cur];
                    return acc;
                },[])
            );
        },

    };

    const check = {
        undo: function() {
            return _undo.length > 0 ? true : false;
        },
        redo: function() {
            return _redo.length > 0 ? true : false;
        },

        checkbox: function(type,nums) {
            if(!check.anyhighlit()) return false;

            const numss = nums === false ?
                find.highlit() :
                nums;

            const states = find.attr(type,numss);
            for(const state of states.values())
                if(state === false)
                    return false;
            return true;
        },
    
        grouped: function() {
            const nums = find.highlit();
            if(!nums) return false;
            const firstrow = find.firsttr();
            for(const num of nums) {
                const cell = find.firsttd(num,firstrow);
                if(cell.classList.contains('group-start') ||
               cell.classList.contains('group-internal') ||
               cell.classList.contains('group-end'))
                    return true;
            }
            return false;
        },
        oneGrouped: function() {
            const nums = find.highlit();
            if(nums.size === 0) return false;
            if(nums.size === 1) {
                const firstrow = find.firsttr();
                const cell = find.firsttd([...nums][0],firstrow);
                //const cell = firstrow.querySelector('td[data-n="'+[...nums][0]+'"]');
                if(cell.classList.contains('group-start') ||
               cell.classList.contains('group-end') ||
               cell.classList.contains('group-internal'))
                    return true;
                else
                    return false;
            }
            return true;
        },

        anyhighlit: function() {
            return _matrix.boxdiv.querySelector('td.highlit') ? true : false;
        },

        manyhighlit: function() {
            return find.highlit().size > 1;
        },

        highlitcell: function() {
            return _matrix.boxdiv.querySelector('td.highlitcell') ? true : false;
        },

        normalizedView: function() {
            return document.getElementById('views').classList.contains('normalized');
        },

        anyNormalized: function() {
            return _normalization || _matrix.boxdiv.querySelector('.lemma[data-normal]');
        },

        headerView: function() {
            return _matrix.boxdiv.querySelector('tr.header').style.display === 'none' ? false : true;
        },
    };

    const make = {
        tei: function(label) {
            const tei = _xml.createElementNS(_teins,'TEI');
            tei.setAttribute('n',label);
            const text = _xml.createElementNS(_teins,'text');
            tei.appendChild(text);
            make.emptywords(text);
            return tei;
        },
        emptywords: function(text,max,start) {
            const m = max || _maxlemma;
            const n_start = start || 0;
            for(let n = n_start; n <= m; n++) {
                const word = _xml.createElementNS(_teins,'w');
                word.setAttribute('n',n);
                text.appendChild(word);
            }
        },
        row: function(label,type) {
            const tr = document.createElement('tr');
            const th = document.createElement('th');
            th.scope = 'row';
            th.draggable = true;
            th.appendChild(document.createTextNode(label));
            th.addEventListener('dragstart',thDragStart);
            tr.dataset.n = label;
            tr.appendChild(th);
            for(let n=0;n<=_maxlemma;n++) {
                const td = document.createElement('td');
                td.dataset.n = n;
                td.className = 'lemma';
                if(type) td.classList.add(type);
                tr.appendChild(td);
            }
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
            _descs.appendChild(this.descbox);
            if(check.normalizedView())
                view.showNormalized(this.boxdiv);
            _viewdiv.appendChild(this.boxdiv);
        //this.closed = false;
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
            _viewdiv.removeChild(this.boxdiv);
            _descs.removeChild(this.descbox);
            const treeindex = _trees.indexOf(this);
            if(treeindex > -1)
                _trees.splice(treeindex,1);
            const textindex = _textboxes.indexOf(this);
            if(textindex > -1)
                _textboxes.splice(textindex,1);
            //this.closed = true;
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
                _windows.length;
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
            if(this.script === _scripts.length)
                this.script = 0;
            const scripter = this.descbox.querySelector('.scripter');
            if(this.script === 0)
                scripter.innerHTML = 'A';
            else
                scripter.innerHTML = to[_scripts[this.script]]('a');
            if(_scripts[this.script] === 'grantha')
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
                    if(check.normalizedView() && hasNormalized) {
                        const temp = document.createElement('span');
                        temp.appendChild(document.createTextNode(node.dataset.normal));
                        return temp;
                    }
                    else
                        return node.IAST.cloneNode(true);
                }());
                const newnode = this.script === 0 ?
                    tochange :
                    changeScript(tochange,_scripts[this.script]);
                node.innerHTML = '';
                while(newnode.firstChild)
                    node.appendChild(newnode.firstChild);
            }
            if(_scripts[this.script] === 'grantha') 
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
            this.nexml = _treelist.get(this.name).cloneNode(true);
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
            this.boxdiv.addEventListener('mouseover',treeMouseover.bind(this));
            this.boxdiv.addEventListener('click',treeClick);
            this.svgcontainer = document.createElement('div');
            this.svgcontainer.id = this.boxdiv.id + 'container';
            this.boxdiv.appendChild(this.svgcontainer);
            this.boxdiv.myTree = this;

            //const parser = new DOMParser();
            ///this.nexml = parser.parseFromString(_treelist.get(this.name),'text/xml');
            this.calcPaths();
            this.jiggleroot();
            this.findLevels();
            this.labelInternal();
        }
        show() {
            _descs.appendChild(this.descbox);
            _viewdiv.appendChild(this.boxdiv);
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
                        if(ids.indexOf(target) !== -1)
                            return {ancestor: tree.querySelector(`node[id="${source}"]`),
                                child: tree.querySelector(`node[id="${target}"]`)};
                        else if(ids.indexOf(source) !== -1)
                            return {ancestor: tree.querySelector(`node[id="${target}"]`),
                                child: tree.querySelector(`node[id="${source}"]`)};
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
                const reading = find.htmlreading(this.boxdiv.querySelector(`span.tree-lemma[data-id="${label}"]`));
                firstpass.set(taxon,new Set([reading]));
            }
            for(let m=1;m<this.levels.length;m++) { // start at 1 (after taxa)
                for(const [node,children] of this.levels[m]) {
                    const readings = children.map(node => firstpass.get(node));
                    const intersection = find.setIntersection(...readings);
                    const result = intersection.size > 0 ?
                        intersection :
                        find.setUnion(...readings);
                    firstpass.set(node,result);

                }
            }
            return firstpass;

        }

        fitch2(firstpass) {
            const taxa = [...this.nexml.querySelectorAll('node[otu]')];
            const secondpass = new Map();

            for(const [node,children] of this.levels[this.levels.length-1]) {
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
                            const intersection = find.setIntersection(ancestral,childreading);
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
            const colored = this.nexml.evaluate('//nex:node[@color]',this.nexml,this.nsResolver,XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,null);

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
                else if(maxheight < 800) return maxheight;
                else return 800;
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
            const alltexts = [...find.texts()];
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
            const proc = makeXSLTProc(treeXSLT);
            /*        for(const [key,value] of _texts)
            for(const el of this.boxdiv.getElementsByClassName(key)) {
                el.innerHTML = '';
                if(m)
                    el.appendChild(XSLTransformString(
                        value.text.slice(n,parseInt(m)+1).join(' '),
                        proc));
                else
                    el.appendChild(XSLTransformString(
                        value.text[n],
                        proc));
                el.IAST = el.cloneNode(true); // why was this commented out?
            } */
            const texts = find.texts();
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
                        const word = find.firstword(x,text);
                        arr.push(word.innerHTML);
                        if(word.hasAttribute('lemma'))
                            normarr[x-n] = word.getAttribute('lemma');
                        if(word.hasAttribute('emended')) emended = true;
                    }
                    el.IAST.appendChild(XSLTransformString(arr.join(' ').replace(/\s+/g,' ').trim(),proc));
                    if(normarr.length !== 0) {
                        const newarr = arr.slice(0).map((e,i) =>
                            normarr.hasOwnProperty(i) ?
                                normarr[i] :
                                e
                        );
                        const temp = document.createElement('span');
                        temp.appendChild(XSLTransformString(newarr.join(' ').replace(/\s+/g,' ').trim(),proc));
                        el.dataset.normal = temp.innerHTML;
                    }
                    if(emended) el.dataset.emended = true;
                    else if(el.dataset.hasOwnProperty('emended')) delete el.dataset.emended;
                }
                else {
                    const word = find.firstword(n,text);
                    el.IAST.appendChild(XSLTransformString(word.innerHTML,proc));
                    if(word.hasAttribute('lemma'))
                        el.dataset.normal = word.getAttribute('lemma');
                    else
                        delete el.dataset.normal;
                    if(word.hasAttribute('emended')) el.dataset.emended = true;
                    else if(el.dataset.hasOwnProperty('emended')) delete el.dataset.emended;
                }
                if(check.normalizedView() && el.dataset.hasOwnProperty('normal'))
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

            /*        for(const [key,value] of _texts) {
            const lemma = m ?
                multiLemmaConcat(value.text.slice(n,parseInt(m)+1)) :
                makeLgLemma(value.text[n]);
            if(lemma === '')
                if(lemmata.hasOwnProperty(''))
                    lemmata[''].push(key);
                else
                    lemmata[''] = [key];
            else {
                const next_lemma = m ?
                    findNextLemma2(value.text,m) :
                    findNextLemma2(value.text,n);
                const clean = normalize(lemma,next_lemma);
                if(lemmata.hasOwnProperty(clean))
                    lemmata[clean].push(key)
                else lemmata[clean] = [key];

                if(clean !== lemma)
                    aliases[key] = clean;
            }
        } */
            const getReading = check.normalizedView() ?
                function(n,text) {
                    const word = find.firstword(n,text);
                    return word.hasAttribute('lemma') ?
                        word.getAttribute('lemma') :
                        word.textContent;
                } :
                function(n,text) {
                    return find.firstword(n,text).textContent;
                };
            for(const text of find.texts()) {
                const key = text.parentNode.getAttribute('n');
            
                // ignore reconstructions and texts not in current tree
                if(!this.nexml.querySelector(`otu[label="${key}"]`))
                    continue;

                const lemma = m ?
                    multiLemmaConcat(
                    //Array.from(Array(parseInt(m)-n+1).keys(), p => p+n)
                        find.range(n,m).map(x => getReading(x,text))
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
                                console.log(path);
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
    
        colourizeVariants(n,m) {
            const paths = this.analyzeVariants(n,m);
            for(const el of this.boxdiv.querySelectorAll('span.tree-lemma')) {
                const path = paths.hasOwnProperty(el.dataset.id) ?
                    paths[paths[el.dataset.id]] :
                    paths[el.textContent];
                const red = {red: 252, green: 70, blue: 107};
                const blue = {red: 63, green: 94, blue: 251};
                if(path) {
                    el.style.color = pickColour(path.length/this.longest.path.length,blue,red);
                    el.dataset.length = path.length;
                    el.dataset.branch_length = path.branch_length;
                    el.dataset.nodes = path.paths[0].nodes.join(';');
                }
                else {
                    el.style.color = pickColour(1/this.longest.path.length,blue,red);
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
            this.text = find.firsttext(name);
        //this.text = arr.get(name).text;
        //this.name = name;
        }
        init() {
            this.makeTextBox();
            this.makeDescBox();
            this.boxdiv.addEventListener('mouseup',textMouseup);
        }
    
        refresh() {
        //this.text = _texts.get(this.name).text;
        //this.text = find.firsttext(this.name);
            this.boxdiv.innerHTML = '';
            const xslt_proc = makeXSLTProc(lemmaXSLT);
            this.boxdiv.appendChild(xslt_proc.transformToFragment(this.text,document));
            //this.boxdiv.appendChild(XSLTransformElement(this.text,xslt_proc));
            touchUpNode(this.boxdiv);
            for(const lemma of find.lemmata(false,this.boxdiv)) {
                lemma.IAST = lemma.cloneNode(true);
            }
            //this.boxdiv.appendChild(csvToFrag(this.text));
            this.updatescript();
        }

        makeTextBox() {
            const textbox = document.createElement('div');
            textbox.dataset.id = this.name; 
            textbox.classList.add('text-box');
            const xslt_proc = makeXSLTProc(lemmaXSLT);
            textbox.appendChild(xslt_proc.transformToFragment(this.text,document));
            //textbox.appendChild(XSLTransformElement(this.text,xslt_proc));
            touchUpNode(textbox);
            for(const lemma of find.lemmata(false,textbox))
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

            const xslt_proc = makeXSLTProc(matrixXSLT);
            scroller.append(xslt_proc.transformToFragment(_xml,document));
            //scroller.append(XSLTransformElement(_xml.documentElement,xslt_proc));
            for(const th of scroller.getElementsByTagName('th'))
                th.addEventListener('dragstart',thDragStart);

            scroller.addEventListener('dragenter',trDragEnter);
            scroller.addEventListener('dragleave',trDragLeave);
            scroller.addEventListener('dragover',e => e.preventDefault());
            scroller.addEventListener('drop',trDragDrop);
            scroller.addEventListener('mousedown',matrixMousedown);
            //this.boxdiv.append(header);
        
            const head = document.createElement('tr');
            head.classList.add('header');
            const firsttd = document.createElement('td');
            firsttd.classList.add('anchor');
            head.appendChild(firsttd);
            const trs = [...find.trs(scroller)];
            const trwalkers = trs.map(tr => find.trWalker(tr));
            const tds = find.tds(false,trs[0]);

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
            //head.addEventListener('click',matrixHeaderClick);
            this.boxdiv.append(scroller);
        }
    }

    const worker = {
        fitch: function() {
            var mss,levels,num,id;

            const find = {
                setIntersection: function(...sets) {
                    const setA = sets[0];
                    return new Set(
                        [...setA].filter(el => {
                            for(let n=1;n<sets.length;n++) {
                                if(!sets[n].has(el))
                                    return false;
                            }
                            return true;
                        })
                    );
                },

                setUnion: function(...sets) {
                    return new Set(
                        sets.reduce((acc, cur) => {
                            acc = [...acc,...cur];
                            return acc;
                        },[])
                    );
                },
            };

            const fitch1 = function(n,target) {
                const firstpass = new Map();
                for(const taxon of levels[0]) {
                    const reading = mss.get(taxon)[n];
                    firstpass.set(taxon,new Set([reading]));
                }
                for(let m=1;m<levels.length;m++) { // start at 1 (after taxa)
                    for(const [node,children] of levels[m]) {
                        const readings = children.map(node => firstpass.get(node));
                        const intersection = find.setIntersection(...readings);
                    
                        // shortcut, because we only care about one node
                        if(node === target && intersection.size === 1)
                            return [...intersection][0];

                        const result = intersection.size > 0 ?
                            intersection :
                            find.setUnion(...readings);
                        firstpass.set(node,result);

                    }
                }
                return firstpass;

            };

            const fitch2 = function(firstpass,target) {
                const taxa = levels[0];
                const secondpass = new Map();

                for(const [node,children] of levels[levels.length-1]) {
                    secondpass.set(node,firstpass.get(node));
                }

                for(let n=levels.length-1;n>1;n--) {
                    for(const [node,children] of levels[n]) {
                        const ancestral = secondpass.get(node);
                        for(const child of children) {
                            if(taxa.indexOf(child) !== -1)
                                continue;
                            const childreading = firstpass.get(child);
                            const intersection = find.setIntersection(ancestral,childreading);
                            const result = intersection.size > 0 ?
                                intersection :
                                childreading;
                        
                            // if reading of target node found, skip rest of reconstruction
                            if(child === target)
                                return result;

                            secondpass.set(child,result);
                        }
                    }
                }
                return secondpass.get(target);
            };

            const fitch = function(target,n) {
                const firstpass = fitch1(n,target);
                if(typeof firstpass === 'string') {
                    return firstpass;
                }
            
                // do the second pass if the first pass is inconclusive
                const formatOutput = function(m) {
                    if(m.size === 1) return [...m][0].trim();
                    const output = [...m].map(str => str.trim() === '' ? '_' : str);
                    return output.length === 1 ? output[0] : '{' + output.join(', ') + '}';
                };
                return formatOutput(fitch2(firstpass,target));

            };

            onmessage = function(e) {
                mss = e.data.readings;
                levels = e.data.levels;
                num = e.data.num;
                id = e.data.id;
                postMessage({n: num, result: fitch(id,num)});
            };
        }
    };

    return {
        slaveinit: function() {
            comboView.init();
            if(window.startbox !== undefined) {
                if(startbox.tree)
                    newBox.tree(startbox.tree.stemmaid,startbox.tree.id);
                else if(startbox.text)
                    newBox.text(startbox.text.name,startbox.text.map);
            }
        },
        maininit: function() {
            document.getElementById('comboview').style.display = 'block';
            comboView.init();
            fillSelector();
            document.getElementById('file').addEventListener('change',fileSelect.bind(null,csvOrXml),false);
        },
        init: function() {
        /*
        _texts = new Map(
            VPTexts.map(o => {
            o[1].text = o[1].text.split(';');
            return o;
            })
        );*/
            _viewdiv = document.getElementById('views');
            _descs = document.getElementById('descs');
            _viewdiv.addEventListener('click',textClick);
            //        _viewdiv.addEventListener('mouseover',lemmaMouseover);
            document.addEventListener('keydown',keyDown);
            document.addEventListener('contextmenu',rightClick);
            document.addEventListener('mouseup',contextMenu.remove);
        },
        getWindows: function() {
            return _windows;
        },
        addWindow: function(win) {
            _windows.push(win);
        },
        getViewdiv: function() {
            return _viewdiv;
        },
        getTrees: function() {
            return _trees;
        },
    };

}());

window.comboView = comboView;
