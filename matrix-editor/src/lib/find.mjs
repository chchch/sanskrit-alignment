const find = function(_state) {
    this.basename = function() {
        return _state.filename.split(/\.[^.]+$/)[0];
    };
    this.range = function(a,b) {
        return Array.from(Array(parseInt(b)-parseInt(a)+1).keys(), x => x+a);
    };
    this.maxlemma = function() {
        return [...this.firsttext().querySelectorAll('w[n]')].length-1;
    };
    this.lemmata = function(num,par) {
        const el = par ? par : document.querySelector('#views');
        return num === false ?
            el.querySelectorAll('.lemma') :
            el.querySelectorAll(`.lemma[data-n="${num}"]`);
    };

    this.tds = function(num,row) {
        const el = row ? row : _state.matrix.boxdiv;
        if(num === false)
            return el.querySelectorAll('td[data-n]');
        else
            return el.querySelectorAll(`td[data-n="${num}"]`);
    };

    this.firsttd = function(num,row) {
        const el = row ? row : _state.matrix.boxdiv;
        return el.querySelector(`td[data-n="${num}"]`);
    };

    this.tr = function(label) {
        return _state.matrix.boxdiv.querySelector(`tr[data-n="${label}"]`);
    };

    this.trs = function(element) {
        const el = element ? element : _state.matrix.boxdiv;
        return el.querySelectorAll('tr[data-n]');
    };

    this.firsttr = function(element) {
        const el = element ? element : _state.matrix.boxdiv;
        return el.querySelector('tr[data-n]');
    };
    this.lasttr = function(element) {
        const el = element ? element : _state.matrix.boxdiv;
        return el.querySelector('tr[data-n]:last-of-type');
    };

    this.trWalker = function(tr) {
        return document.createNodeIterator(tr,NodeFilter.SHOW_ELEMENT,
            {acceptNode: function(node) {if(node.tagName === 'TD') return NodeFilter.FILTER_ACCEPT;}},
            false);
    };

    this.textWalker = function(text) {
        return document.createNodeIterator(text,NodeFilter.SHOW_ELEMENT,
            {acceptNode: function(node) {if(node.tagName.toLowerCase() === 'w') return NodeFilter.FILTER_ACCEPT;}},
            false);
    };

    this.tei = function(label, doc = _state.xml) {
        return doc.querySelector(`TEI[n="${label}"]`);
    };

    this.teis = function(doc = _state.xml) {
        return doc.querySelectorAll('TEI');
    };

    this.texts = function(element) {
        const el = element ? element : _state.xml;
        return el.querySelectorAll('text');
    };

    this.serializedtexts = function(tree, normalized) {
        const otus = [...tree.querySelectorAll('otu[label]')].map(el => el.getAttribute('label'));
        const teis = [...this.teis()].filter(el => otus.indexOf(el.getAttribute('n')) !== -1);
        return new Map(
            teis.map(t => [
                tree.querySelector(`node[label="${t.getAttribute('n')}"]`).getAttribute('id'),
                [...t.querySelectorAll('w')].map(w => {
                    return normalized && w.hasAttribute('lemma') ?
                        w.getAttribute('lemma') :
                        w.textContent;
                })
            ])
        );
    };

    this.serializedlevels = function(levels) {
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
    };

    this.firsttext = function(id) {
        return !id ? 
            _state.xml.querySelector('text') :
            _state.xml.querySelector(`[n="${id}"] text`);
    };

    this.words = function(num,text) {
        const el = text ? text : _state.xml;
        if(num === false)
            return el.querySelectorAll('w[n]');
        else
            return el.querySelectorAll(`w[n="${num}"]`);
    };

    this.firstword = function(num,row) {
        const el = row ? row : _state.xml;
        return el.querySelector(`w[n="${num}"]`);
    };

    this.normal = function(el) {
        const par = el ? el : document.getElementById('views');
        //const par = el ? el : _state.matrix.boxdiv;
        return par.querySelectorAll('.lemma[data-normal], .tree-lemma[data-normal]');
    };

    this.htmlreading = function(el, normalized) {
        return normalized && el.dataset.normal ?
            el.dataset.normal :
            el.IAST.textContent;
    };
/*
    this.xmlreading = function(label,n,normalized) {
        const el = _state.xml.querySelector(`TEI[n="${label}"] > text > w[n="${n}"]`);
        return normalized && el.hasAttribute('lemma') ?
            el.getAttribute('lemma') :
            el.textContent;
    };

    this.xmlreadings = function(label, normalized) {
        const els = [..._state.xml.querySelectorAll(`TEI[n="${label}"] > text > w`)];

        return els.map(el => {
            normalized && el.hasAttribute('lemma') ?
                el.getAttribute('lemma') :
                el.textContent;
        });
    };
*/
    this.ths = function() {
        return _state.matrix.boxdiv.querySelectorAll('th[data-ref]');
    };

    this.firstth = function(num) {
        return _state.matrix.boxdiv.querySelector(`th[data-ref="${num}"]`);
    };
    this.checkbox = function(num,type) {
        return _state.matrix.boxdiv.querySelector(`th[data-ref="${num}"] input.${type}`);
    };

    this.highlit = function() {
        const firstrow = this.firsttr();
        const lemmata = firstrow.querySelectorAll('.highlit');
        //if(lemmata.length === 0) return false;
        const nums = new Set();
        for(const lemma of lemmata) {
            nums.add(lemma.dataset.n);
        }
        return nums;
    };

    this.highlitcell = function() {
        return _state.matrix.boxdiv.querySelector('td.highlitcell');
    };

    this.highlitrow = function() {
        const highlitcell = this.highlitcell();
        return highlitcell ? highlitcell.closest('tr') : false;
    };

    this.lowhigh = function(nums) {
        const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
        const low = parseInt(sortednums[0]);
        const high = sortednums.length > 1 ?
            parseInt(sortednums[sortednums.length-1]) :
            undefined;
        return [low,high];
    };

    this.readings = function(num, element) {
        const el = element ? element : _state.matrix.boxdiv;
        const tds = this.tds(num,el);
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
    };

    this.attr = function(type,nums) {
        const firstrow = this.firsttr();
        var states = [];
        for(const num of nums) {
            const cell = this.firsttd(num,firstrow);
            if(cell.dataset[type] === 'true')
                states.push([num,true]);
            else
                states.push([num,false]);
        }
        return new Map(states);
    };

    this.whichattr = function(el) {
        if(el.hasAttribute('n')) return 'n';
        else if(el.hasAttribute('data-n')) return 'data-n';
        else if(el.hasAttribute('data-ref')) return 'data-ref';
        else return false;
    };

    this.clauses = function(nums,strict = false) {
        const firstrow = this.firsttext();
        var someungrouped = false;

        // make a list of clauses
        const cls = new Set();
        for(const num of nums) {
            const word = this.firstword(num,firstrow);
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
    };

    this.clausesToRemove = function(clgroups,nums,threshold = 0) {
        const toremove = new Set();
        for(const group of clgroups) {
            const clone = new Set(group);
            for(const num of nums)
                clone.delete(num);
            if(clone.size <= threshold)
                toremove.add(group);
        }
        return [...toremove];
    };

    this.empty = function() {
        const emptyset = new Set();
        const trs = [...this.trs()];
        const trWalkers = trs.map(el => this.trWalker(el));
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
    };

    this.prevNonempty = function(index,arr) {
        for(let n=index;n>=0;n--) {
            const td = arr[n];
            if(td.textContent !== '') return n;
        }
        return false;
        /*         var n = num;
    var td = this.firsttd(n,row);
    while(td) {
        if(td.textContent !== '') return n;
        n--;
        td = this.firsttd(n,row);
    }
    return num; */
    };

    this.nextNonempty = function(index,arr) {
        for(let n=index;n<arr.length;n++) {
            const td = arr[n];
            if(td.textContent !== '') return n;
        }
        return false;
    };

    this.setIntersection = function(...sets) {
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
    };

    this.setUnion = function(...sets) {
        return new Set(
            sets.reduce((acc, cur) => {
                acc = [...acc,...cur];
                return acc;
            },[])
        );
    };

    this.cursorPos = function(el) {
        const range = window.getSelection().getRangeAt(0);
        const preCaretRange = range.cloneRange();
        preCaretRange.selectNodeContents(el);
        const fullLength = preCaretRange.toString().length;
        preCaretRange.setEnd(range.endContainer, range.endOffset);
        const caretOffset = preCaretRange.toString().length;
        return [caretOffset,fullLength];
    };

    this.selection = function() {    
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
    };
};

export { find };
