import { Sanscript } from './sanscript.mjs';

'use strict';

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
        const finals_regex = new RegExp('(['+[...finals.keys()].join('')+'])്(?![^\u200C ,—’―])','g'); 

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

export { changeScript };
