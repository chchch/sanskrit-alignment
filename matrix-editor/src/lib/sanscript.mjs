/**
 * Sanscript
 *
 * Sanscript is a Sanskrit transliteration library. Currently, it supports
 * other Indian languages only incidentally.
 *
 * Released under the MIT and GPL Licenses.
 */

const Sanscript = {};

'use strict';

Sanscript.defaults = {
    skip_sgml: false,
    syncope: false
};

var schemes = Sanscript.schemes = {
        tamil: {
            vowels: ['அ','ஆ',
            'இ','ஈ',
            'உ','ஊ',
            '𑌋','𑍠',
            '𑌌','𑍡',
            'எ','ஏ','ஐ',
            'ஒ','ஓ','ஔ'],
            vowel_marks: ['ா',
            'ி','ீ',
            'ு','ூ',
            '𑍃','𑍄',
            '𑍢','𑍣',
            'ெ','ே','ை',
            'ொ','ோ','ௌ'],
            other_marks: ['𑌂','𑌃','𑌁','','','ஃ'], // ṃ ḥ m̐ ẖ ḫ ḵ 
            virama: ['்'],
            consonants: ['க','𑌖','𑌗','𑌘','ங',
            'ச','𑌛','ஜ','𑌝','ஞ',
            'ட','𑌠','𑌡','𑌢','ண',
            'த','𑌥','𑌦','𑌧','ந',
            'ப','𑌫','𑌬','𑌭','ம',
            'ய','ர','ல','வ',
            'ஶ','ஷ','ஸ','ஹ',
            'ள','ழ','ற','ன'],
            symbols: ['௦','௧','௨','௩','௪','௫','௬','௭','௮','௯','ௐ','','𑌽','।','॥','௰','௱','௲'],
        },
        
        grantha: {
            vowels: ['𑌅','𑌆',
                '𑌇','𑌈',
                '𑌉','𑌊',
                '𑌋','𑍠',
                '𑌌','𑍡',
                'எ','𑌏','𑌐',
                'ஒ','𑌓','𑌔'
              
            ],
            vowel_marks: ['𑌾',
                '𑌿','𑍀',
                '𑍁','𑍂',
                '𑍃','𑍄',
                '𑍢','𑍣',
                'ெ','𑍇','𑍈',
                'ொ','𑍋','𑍌'
            ],
            other_marks: ['𑌂','𑌃','𑌁','𑍞','𑍟','ஃ'], // ṃ ḥ m̐ ẖ ḫ ḵ 
            virama: ['𑍍'],
            consonants: ['𑌕','𑌖','𑌗','𑌘','𑌙',
                '𑌚','𑌛','𑌜','𑌝','𑌞',
                '𑌟','𑌠','𑌡','𑌢','𑌣',
                '𑌤','𑌥','𑌦','𑌧','𑌨',
                '𑌪','𑌫','𑌬','𑌭','𑌮',
                '𑌯','𑌰','𑌲','𑌵',
                '𑌶','𑌷','𑌸','𑌹',
                '𑌳','ழ','ற','ன',
            ],
            symbols: ['௦','௧','௨','௩','௪','௫','௬','௭','௮','௯','𑍐','','𑌽','।','॥','௰','௱','௲'],
        },
        
        malayalam: {
            vowels: ['അ','ആ',
                'ഇ','ഈ',
                'ഉ','ഊ',
                'ഋ','ൠ',
                'ഌ','ൡ',
                'എ','ഏ','ഐ',
                'ഒ','ഓ','ഔ'
            ],
            vowel_marks: ['ാ',
                'ി','ീ',
                'ു','ൂ',
                'ൃ','ൄ',
                'ൢ','ൣ',
                'െ','േ','ൈ',
                'ൊ','ോ','ൌ'
            ],
            other_marks: ['ം','ഃ','ഁ','','',''],
            virama: ['്'],
            consonants: ['ക','ഖ','ഗ','ഘ','ങ',
                'ച','ഛ','ജ','ഝ','ഞ',
                'ട','ഠ','ഡ','ഢ','ണ',
                'ത','ഥ','ദ','ധ','ന',
                'പ','ഫ','ബ','ഭ','മ',
                'യ','ര','ല','വ',
                'ശ','ഷ','സ','ഹ',
                'ള','ഴ','റ','ഩ'
            ],
            symbols: ['൦','൧','൨','൩','൪','൫','൬','൭','൮','൯','ഒം','','ഽ','।','॥','൰','൱','൲'],
        },
        
        newa: {
            vowels: ['𑐀','𑐁',
                '𑐂','𑐃',
                '𑐄','𑐅',
                '𑐆','𑐇',
                '𑐈','𑐉',
                '','𑐊','𑐋',
                '','𑐌','𑐍'
            ],
            vowel_marks: ['𑐵',
                '𑐶','𑐷',
                '𑐸','𑐹',
                '𑐺','𑐻',
                '𑐼','𑐽',
                '','𑐾','𑐿',
                '','𑑀','𑑁',
            ],
            other_marks: ['𑑄','𑑅','𑑃','𑑠','𑑡',''],
            virama: ['𑑂'],
            consonants: ['𑐎','𑐏','𑐐','𑐑','𑐒',
                '𑐔','𑐕','𑐖','𑐗','𑐘',
                '𑐚','𑐛','𑐜','𑐝','𑐞',
                '𑐟','𑐠','𑐡','𑐢','𑐣',
                '𑐥','𑐦','𑐧','𑐨','𑐩',
                '𑐫','𑐬','𑐮','𑐰',
                '𑐱','𑐲','𑐳','𑐴'
            ],
            symbols: ['𑑐','𑑑','𑑒','𑑓','𑑔','𑑕','𑑖','𑑗','𑑘','𑑙',
            '𑑉','','𑑇','𑑋','𑑌']
        },
        
        sarada: {
            vowels: ['𑆃','𑆄',
                '𑆅','𑆆',
                '𑆇','𑆈',
                '𑆉','𑆊',
                '𑆋','𑆌',
                '','𑆍','𑆎',
                '','𑆏','𑆐'
            ],
            vowel_marks: ['𑆳',
                '𑆴','𑆵',
                '𑆶','𑆷',
                '𑆸','𑆹',
                '𑆺','𑆻',
                '','𑆼','𑆽',
                '','𑆾','𑆿'
            ],
            other_marks: ['𑆁','𑆂','𑆀','𑇁','𑇂',''],
            virama: ['𑇀'],
            consonants: ['𑆑','𑆒','𑆓','𑆔','𑆕',
                '𑆖','𑆗','𑆘','𑆙','𑆚',
                '𑆛','𑆜','𑆝','𑆞','𑆟',
                '𑆠','𑆡','𑆢','𑆣','𑆤',
                '𑆥','𑆦','𑆧','𑆨','𑆩',
                '𑆪','𑆫','𑆬','𑆮',
                '𑆯','𑆰','𑆱','𑆲',
                '𑆭'
            ],
            symbols: ['𑇐','𑇑','𑇒','𑇓','𑇔','𑇕','𑇖','𑇗','𑇘','𑇙',
                '𑇄','','𑇁','𑇅','𑇆']
        },

        nandinagari: {
            vowels: ['\u{119A0}','\u{119A1}',
                '\u{119A2}','\u{119A3}',
                '\u{1194}','\u{119A5}',
                '\u{119A6}','\u{119A7}',
                '\u{119C9}\u{119D6}','\u{119C9}\u{119D7}',
                '','\u{119AA}','\u{119AB}',
                '','\u{119AC}','\u{119AD}'
            ],
            vowel_marks: ['\u{119D1}',
                '\u{119D2}','\u{119D3}',
                '\u{119D4}','\u{119D5}',
                '\u{119D6}','\u{119D7}',
                '\u{119C9}\u{119D6}','\u{119C9}\u{119D7}',
                '','\u{119DA}','\u{119DB}',
                '','\u{119DC}','\u{119DD}'
            ],
            other_marks: ['\u{119DE}','\u{119DF}','','',''],
            virama: ['\u{119E0}'],
            consonants: ['\u{119AE}','\u{119AF}','\u{119B0}','\u{119B1}','\u{11B2}',
                '\u{119B3}','\u{119B4}','\u{119B5}','\u{119B6}','\u{119B7}',
                '\u{119B8}','\u{119B9}','\u{119BA}','\u{119BB}','\u{119BC}',
                '\u{119BD}','\u{119BE}','\u{119BF}','\u{119C0}','\u{119C1}',
                '\u{119C2}','\u{119C3}','\u{119C4}','\u{119C5}','\u{119C6}',
                '\u{119C7}','\u{119C8}','\u{119C9}','\u{119CA}',
                '\u{119CB}','\u{119CC}','\u{119CD}','\u{119CE}',
                '\u{119CF}','','\u{119D0}'
            ],
            // use Kannada numerals & Devanagari daṇḍas
            symbols: ['೦','೧','೨','೩','೪','೫','೬','೭','೮','೯',
                '\u{119AC}\u{119DE}','','\u{119E1}','।','॥']
        },

        bengali: {
            vowels: ['অ','আ',
            'ই','ঈ',
            'উ','ঊ',
            'ঋ','ৠ',
            'ঌ','ৡ',
            '','এ','ঐ',
            '','ও','ঔ'],
            vowel_marks: ['া',
            'ি','ী',
            'ু','ূ',
            'ৃ','ৄ',
            'ৢ','ৣ',
            '','ে','ৈ',
            'ো','ৌ'],
            other_marks: ['ং','ঃ','ঁ','','',''],
            virama: ['্'],
            consonants: ['ক','খ','গ','ঘ','ঙ',
            'চ','ছ','জ','ঝ','ঞ',
            'ট','ঠ','ড','ঢ','ণ',
            'ত','থ','দ','ধ','ন',
            'প','ফ','ব','ভ','ম',
            'য','র','ল','ব',
            'শ','ষ','স','হ',
            'ळ','','','',
            'য়',
            '','','','','','','ড়','ঢ়' // (q qh ġ z zh f) ṙ ṙh (ṫh ḋh w)
            ],
            symbols: ['০','১','২','৩','৪','৫','৬','৭','৮','৯','ওঁ','','ঽ','।','॥'],
        },
        devanagari: {
            vowels: ['अ','आ', // a ā
            'इ','ई', // i ī
            'उ','ऊ', // u ū
            'ऋ','ॠ', // ṛ ṝ
            'ऌ','ॡ', // l̥ l̥̄
            'ऎ','ए','ऐ', // e ē ai
            'ऒ','ओ','औ' // o ō au
            ],
            vowel_marks: ['ा', // ā
            'ि','ी', // i ī
            'ु','ू', // u ū
            'ृ','ॄ', // ṛ ṝ
            'ॢ','ॣ', // l̥ l̥̄
            'ॆ','े','ै', // e ē ai
            'ॊ','ो','ौ', // o ō au
            'ॎ','ॎे','ॎा','ॎो' // e ai o au
            ],
            
            other_marks: ['ं','ः','ँ','ᳵ','ᳶ',''], // ṃ ḥ m̐ ẖ ḫ ḵ 

            virama: ['्'],

            consonants: ['क','ख','ग','घ','ङ',
            'च','छ','ज','झ','ञ',
            'ट','ठ','ड','ढ','ण',
            'त','थ','द','ध','न',
            'प','फ','ब','भ','म',     
            'य','र','ल','व',
            'श','ष','स','ह',
            'ळ','ऴ','ऱ','ऩ',
            'य़',
            'क़','ख़','ग़','ज़','झ़','फ़','ड़','ढ़','थ़','ध़','व़' // q qh ġ z zh f ṙ ṙh ṫh ḋh w
            ],

            symbols: ['०','१','२','३','४','५','६','७','८','९','ॐ','ꣽ','ऽ','।','॥'],

            zwj: ['\u200D'],

            // Dummy consonant. This is used in ITRANS to prevert certain types
            // of parser ambiguity. Thus "barau" -> बरौ but "bara_u" -> बरउ.
            skip: [''],

            // Vedic accent. Udatta and anudatta.
            accent: ['\u0951', '\u0952'],

            // Accent combined with anusvara and and visarga. For compatibility
            // with ITRANS, which allows the reverse of these four.
            combo_accent: 'ः॑ ः॒ ं॑ ं॒'.split(' '),
        },
        telugu: {
            vowels: ['అ','ఆ', // a ā
            'ఇ','ఈ', // i ī
            'ఉ','ఊ', // u ū
            'ఋ','ౠ', // ṛ ṝ
            'ఌ','ౡ', // l̥ l̥̄
            'ఎ','ఏ','ఐ', // e ē ai
            'ఒ','ఓ','ఔ' // o ō au
            ],
            vowel_marks: ['ా', // ā
            'ి','ీ', // i ī
            'ు','ూ', // u ū
            'ృ','ౄ', // ṛ r̄,
            'ౢ','ౣ', // l̥ l̥̄
            'ె','ే','ై', // e ē ai
            'ొ','ో','ౌ' // o ō au
            ],
            other_marks: ['ం','ః','ఀ','','',''], // ṃ ḥ m̐ ẖ ḫ ḵ (what about ardhānusvāra?)
            virama: ['్'],
            consonants: ['క','ఖ','గ','ఘ','ఙ', // k kh g gh ṅ
            'చ','ఛ','జ','ఝ','ఞ', // c ch j jh ñ
            'ట','ఠ','డ','ఢ','ణ', // ṭ ṭh ḍ ḍh ṇ
            'త','థ','ద','ధ','న', // t th d dh n
            'ప','ఫ','బ','భ','మ', // p ph b bh m
            'య','ర','ల','వ', // y r l v
            'శ','ష','స','హ', // ś ṣ s h
            'ళ','ఴ','ఱ'], // ḷ ḻ ṟ
            symbols: ['౦','౧','౨','౩','౪','౫','౬','౭','౮','౯','ఓం','','ఽ','।','॥'],
        },

        iast: {
            vowels: ['a','ā',
            'i','ī',
            'u','ū',
            'ṛ','ṝ',
            'l̥','l̥̄',
            'e','ē','ai',
            'o','ō','au',
            'ê','aî','ô','aû'], // Devanāgarī pṛṣthamātrās
            other_marks: ['ṃ','ḥ','m̐','ẖ','ḫ','ḵ'],
            virama: [''],
            consonants: ['k','kh','g','gh','ṅ',
            'c','ch','j','jh','ñ',
            'ṭ','ṭh','ḍ','ḍh','ṇ',
            't','th','d','dh','n',
            'p','ph','b','bh','m',
            'y','r','l','v',
            'ś','ṣ','s','h',
            'ḷ','ḻ','ṟ','ṉ', // Dravidian
            'ẏ', // Bengali
            'q','qh','ġ','z','zh','f','ṙ','ṙh','ṫh','ḋh','w'],
            symbols: ['0','1','2','3','4','5','6','7','8','9','oṁ','oḿ','\'','|','||','⁰','⁰⁰','⁰⁰⁰'],
        }
    },

    // Set of names of schemes
    romanSchemes = {},

    // Map of alternate encodings.
    allAlternates = {
    },

    // object cache
    cache = {};

/**
 * Check whether the given scheme encodes romanized Sanskrit.
 *
 * @param name  the scheme name
 * @return      boolean
 */
Sanscript.isRomanScheme = function(name) {
    return romanSchemes.hasOwnProperty(name);
};

/**
 * Add a Brahmic scheme to Sanscript.
 *
 * Schemes are of two types: "Brahmic" and "roman". Brahmic consonants
 * have an inherent vowel sound, but roman consonants do not. This is the
 * main difference between these two types of scheme.
 *
 * A scheme definition is an object ("{}") that maps a group name to a
 * list of characters. For illustration, see the "devanagari" scheme at
 * the top of this file.
 *
 * You can use whatever group names you like, but for the best results,
 * you should use the same group names that Sanscript does.
 *
 * @param name    the scheme name
 * @param scheme  the scheme data itself. This should be constructed as
 *                described above.
 */
Sanscript.addBrahmicScheme = function(name, scheme) {
    Sanscript.schemes[name] = scheme;
};

/**
 * Add a roman scheme to Sanscript.
 *
 * See the comments on Sanscript.addBrahmicScheme. The "vowel_marks" field
 * can be omitted.
 *
 * @param name    the scheme name
 * @param scheme  the scheme data itself
 */
Sanscript.addRomanScheme = function(name, scheme) {
    if (!('vowel_marks' in scheme)) {
        scheme.vowel_marks = scheme.vowels.slice(1);
    }
    Sanscript.schemes[name] = scheme;
    romanSchemes[name] = true;
};

/**
 * Create a deep copy of an object, for certain kinds of objects.
 *
 * @param scheme  the scheme to copy
 * @return        the copy
 */
/*
var cheapCopy = function(scheme) {
    var copy = {};
    for (var key in scheme) {
        if (!scheme.hasOwnProperty(key)) {
            continue;
        }
        copy[key] = scheme[key].slice(0);
    }
    return copy;
};
*/
// Set up various schemes
(function() {
    // Set up roman schemes
    /*
    var kolkata = schemes.kolkata = cheapCopy(schemes.iast),
        schemeNames = 'iast itrans hk kolkata slp1 velthuis wx'.split(' ');
    kolkata.vowels = 'a ā i ī u ū ṛ ṝ ḷ ḹ e ē ai o ō au'.split(' ');
    */
    var schemeNames = ['iast'];
    // These schemes already belong to Sanscript.schemes. But by adding
    // them again with `addRomanScheme`, we automatically build up
    // `romanSchemes` and define a `vowel_marks` field for each one.
    for (var i = 0, name; (name = schemeNames[i]); i++) {
        Sanscript.addRomanScheme(name, schemes[name]);
    }
    /*
    // ITRANS variant, which supports Dravidian short 'e' and 'o'.
    var itrans_dravidian = cheapCopy(schemes.itrans);
    itrans_dravidian.vowels = 'a A i I u U Ri RRI LLi LLi e E ai o O au'.split(' ');
    itrans_dravidian.vowel_marks = itrans_dravidian.vowels.slice(1);
    allAlternates.itrans_dravidian = allAlternates.itrans;
    Sanscript.addRomanScheme('itrans_dravidian', itrans_dravidian);
*/
}());

/**
 * Create a map from every character in `from` to its partner in `to`.
 * Also, store any "marks" that `from` might have.
 *
 * @param from     input scheme
 * @param to       output scheme
 * @param options  scheme options
 */
var makeMap = function(from, to, /*options*/) {
    var alternates = allAlternates[from] || {},
        consonants = {},
        fromScheme = Sanscript.schemes[from],
        letters = {},
        tokenLengths = [],
        marks = {},
        toScheme = Sanscript.schemes[to];

    for (var group in fromScheme) {
        if (!fromScheme.hasOwnProperty(group)) {
            continue;
        }
        var fromGroup = fromScheme[group],
            toGroup = toScheme[group];
        if (toGroup === undefined) {
            continue;
        }
        for (var i = 0; i < fromGroup.length; i++) {
            var F = fromGroup[i],
                T = toGroup[i],
                alts = alternates[F] || [],
                numAlts = alts.length,
                j = 0;

            tokenLengths.push(F.length);
            for (j = 0; j < numAlts; j++) {
                tokenLengths.push(alts[j].length);
            }

            if (group === 'vowel_marks' || group === 'virama') {
                marks[F] = T;
                for (j = 0; j < numAlts; j++) {
                    marks[alts[j]] = T;
                }
            } else {
                letters[F] = T;
                for (j = 0; j < numAlts; j++) {
                    letters[alts[j]] = T;
                }
                if (group === 'consonants' || group === 'other') {
                    consonants[F] = T;

                    for (j = 0; j < numAlts; j++) {
                        consonants[alts[j]] = T;
                    }
                }
            }
        }
    }
    return {consonants: consonants,
        fromRoman: Sanscript.isRomanScheme(from),
        letters: letters,
        marks: marks,
        maxTokenLength: Math.max.apply(Math, tokenLengths),
        toRoman: Sanscript.isRomanScheme(to),
        virama: toScheme.virama};
};

/**
 * Transliterate from a romanized script.
 *
 * @param data     the string to transliterate
 * @param map      map data generated from makeMap()
 * @param options  transliteration options
 * @return         the finished string
 */
var transliterateRoman = function(data, map, options) {
    var buf = [],
        consonants = map.consonants,
        dataLength = data.length,
        hadConsonant = false,
        letters = map.letters,
        marks = map.marks,
        maxTokenLength = map.maxTokenLength,
        optSkipSGML = options.skip_sgml,
        optSyncope = options.syncope,
        tempLetter,
        tempMark,
        tokenBuffer = '',
        toRoman = map.toRoman,
        virama = map.virama;

    // Transliteration state. It's controlled by these values:
    // - `skippingSGML`: are we in SGML?
    // - `toggledTrans`: are we in a toggled region?
    //
    // We combine these values into a single variable `skippingTrans`:
    //
    //     `skippingTrans` = skippingSGML || toggledTrans;
    //
    // If (and only if) this value is true, don't transliterate.
    var skippingSGML = false,
        skippingTrans = false,
        toggledTrans = false;

    for (var i = 0, L; (L = data.charAt(i)) || tokenBuffer; i++) {
        // Fill the token buffer, if possible.
        var difference = maxTokenLength - tokenBuffer.length;
        if (difference > 0 && i < dataLength) {
            tokenBuffer += L;
            if (difference > 1) {
                continue;
            }
        }

        // Match all token substrings to our map.
        for (var j = 0; j < maxTokenLength; j++) {
            var token = tokenBuffer.substr(0,maxTokenLength-j);

            if (skippingSGML === true) {
                skippingSGML = (token !== '>');
            } else if (token === '<') {
                skippingSGML = optSkipSGML;
            } else if (token === '##') {
                toggledTrans = !toggledTrans;
                tokenBuffer = tokenBuffer.substr(2);
                break;
            }
            skippingTrans = skippingSGML || toggledTrans;
            if ((tempLetter = letters[token]) !== undefined && !skippingTrans) {
                if (toRoman) {
                    buf.push(tempLetter);
                } else {
                    // Handle the implicit vowel. Ignore 'a' and force
                    // vowels to appear as marks if we've just seen a
                    // consonant.
                    if (hadConsonant) {
                        if ((tempMark = marks[token])) {
                            buf.push(tempMark);
                        } else if (token !== 'a') {
                            buf.push(virama);
                            buf.push(tempLetter);
                        }
                    } else {
                        buf.push(tempLetter);
                    }
                    hadConsonant = token in consonants;
                }
                tokenBuffer = tokenBuffer.substr(maxTokenLength-j);
                break;
            } else if (j === maxTokenLength - 1) {
                if (hadConsonant) {
                    hadConsonant = false;
                    if (!optSyncope) {
                        buf.push(virama);
                    }
                }
                buf.push(token);
                tokenBuffer = tokenBuffer.substr(1);
                // 'break' is redundant here, "j == ..." is true only on
                // the last iteration.
            }
        }
    }
    if (hadConsonant && !optSyncope) {
        buf.push(virama);
    }
    return buf.join('');
};

/**
 * Transliterate from a Brahmic script.
 *
 * @param data     the string to transliterate
 * @param map      map data generated from makeMap()
 * @param options  transliteration options
 * @return         the finished string
 */
var transliterateBrahmic = function(data, map, /*options*/) {
    var buf = [],
        consonants = map.consonants,
        hadRomanConsonant = false,
        letters = map.letters,
        marks = map.marks,
        dataLength = data.length,
        maxTokenLength = map.maxTokenLength,
        tempLetter,
        tokenBuffer = '',
        toRoman = map.toRoman,
        skippingTrans = false;

    for (var i = 0, L; (L = data.charAt(i)) || tokenBuffer; i++) {
        // Fill the token buffer, if possible.
        var difference = maxTokenLength - tokenBuffer.length;
        if (difference > 0 && i < dataLength) {
            tokenBuffer += L;
            if (difference > 1) {
                continue;
            }
        }

        // Match all token substrings to our map.
        for (var j = 0; j < maxTokenLength; j++) {
            var token = tokenBuffer.substr(0,maxTokenLength-j);

            if((tempLetter = marks[token]) !== undefined && !skippingTrans) {
                buf.push(tempLetter);
                hadRomanConsonant = false;
                tokenBuffer = tokenBuffer.substr(maxTokenLength-j);
                break;
            } 
            else if((tempLetter = letters[token])) {
                if (hadRomanConsonant) {
                    buf.push('a');
                    hadRomanConsonant = false;
                }
                buf.push(tempLetter);
                hadRomanConsonant = toRoman && (token in consonants);
                tokenBuffer = tokenBuffer.substr(maxTokenLength-j);
                break;

            } else if (j === maxTokenLength - 1) {
                if (hadRomanConsonant) {
                    buf.push('a');
                    hadRomanConsonant = false;
                }
                buf.push(token);
                tokenBuffer = tokenBuffer.substr(1);
            }
        }
    }
    if (hadRomanConsonant) {
        buf.push('a');
    }
    return buf.join('');
};

/**
 * Transliterate from one script to another.
 *
 * @param data     the string to transliterate
 * @param from     the source script
 * @param to       the destination script
 * @param options  transliteration options
 * @return         the finished string
 */
Sanscript.t = function(data, from, to, options) {
    options = options || {};
    var cachedOptions = cache.options || {},
        defaults = Sanscript.defaults,
        hasPriorState = (cache.from === from && cache.to === to),
        map;

    // Here we simultaneously build up an `options` object and compare
    // these options to the options from the last run.
    for (var key in defaults) {
        if (defaults.hasOwnProperty(key)) {
            var value = defaults[key];
            if (key in options) {
                value = options[key];
            }
            options[key] = value;

            // This comparison method is not generalizable, but since these
            // objects are associative arrays with identical keys and with
            // values of known type, it works fine here.
            if (value !== cachedOptions[key]) {
                hasPriorState = false;
            }
        }
    }

    if (hasPriorState) {
        map = cache.map;
    } else {
        map = makeMap(from, to, options);
        cache = {
            from: from,
            map: map,
            options: options,
            to: to};
    }
    /*
    // Easy way out for "{\m+}", "\", and ".h".
    if (from === 'itrans') {
        data = data.replace(/\{\\m\+\}/g, '.h.N');
        data = data.replace(/\.h/g, '');
        data = data.replace(/\\([^'`_]|$)/g, '##$1##');
    }
    */
    if (map.fromRoman) {
        return transliterateRoman(data, map, options);
    } else {
        return transliterateBrahmic(data, map, options);
    }
};

export { Sanscript };
