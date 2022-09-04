const Utils = function(_state) {
    const find = {
        curxml() {
            return _state.xml;
        },
        teins() {
            return _state.teins;
        },
        basename() {
            return _state.filename.split(/\.[^.]+$/)[0];
        },
        range(a,b) {
            return Array.from(Array(parseInt(b)-parseInt(a)+1).keys(), x => x+a);
        },
        maxlemma() {
            return [...find.firsttext().querySelectorAll('w[n]')].length-1;
        },
        lemmata(num,par) {
            const el = par ? par : document.querySelector('#views');
            return num === false ?
                el.querySelectorAll('.lemma') :
                el.querySelectorAll(`.lemma[data-n="${num}"]`);
        },

        tds(num,row) {
            const el = row ? row : _state.matrix.boxdiv;
            if(num === false)
                return el.querySelectorAll('td[data-n]');
            else
                return el.querySelectorAll(`td[data-n="${num}"]`);
        },

        firsttd(num,row) {
            const el = row ? row : _state.matrix.boxdiv;
            return el.querySelector(`td[data-n="${num}"]`);
        },

        tr(label) {
            return _state.matrix.boxdiv.querySelector(`tr[data-n="${label}"]`);
        },

        trs(element) {
            const el = element ? element : _state.matrix.boxdiv;
            return el.querySelectorAll('tr[data-n]');
        },

        firsttr(element) {
            const el = element ? element : _state.matrix.boxdiv;
            return el.querySelector('tr[data-n]');
        },
        lasttr(element) {
            const el = element ? element : _state.matrix.boxdiv;
            return el.querySelector('tr[data-n]:last-of-type');
        },

        trWalker(tr) {
            return document.createNodeIterator(tr,NodeFilter.SHOW_ELEMENT,
                {acceptNode: function(node) {if(node.tagName === 'TD') return NodeFilter.FILTER_ACCEPT;}},
                false);
        },

        textWalker(text) {
            return document.createNodeIterator(text,NodeFilter.SHOW_ELEMENT,
                {acceptNode: function(node) {if(node.tagName.toLowerCase() === 'w') return NodeFilter.FILTER_ACCEPT;}},
                false);
        },

        tei(label, doc = _state.xml) {
            return doc.querySelector(`TEI[n="${label}"]`);
        },

        teis(doc = _state.xml) {
            return doc.querySelectorAll('TEI');
        },

        texts(element) {
            const el = element ? element : _state.xml;
            return el.querySelectorAll('text');
        },

        serializedtexts(tree, normalized) {
            const otus = [...tree.querySelectorAll('otu[label]')].map(el => el.getAttribute('label'));
            const teis = [...find.teis()].filter(el => otus.indexOf(el.getAttribute('n')) !== -1);
            return teis.map(t => [
                    tree.querySelector(`node[label="${t.getAttribute('n')}"]`).getAttribute('id'),
                    [...t.querySelectorAll('w')].map(w => {
                        return normalized && w.hasAttribute('lemma') ?
                            w.getAttribute('lemma') :
                            w.textContent;
                    })
                ]);
        },

        serializedlevels(levels) {
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

        firsttext(id) {
            return !id ? 
                _state.xml.querySelector('text') :
                _state.xml.querySelector(`[n="${id}"] text`);
        },

        words(num,text) {
            const el = text ? text : _state.xml;
            if(num === false)
                return el.querySelectorAll('w[n]');
            else
                return el.querySelectorAll(`w[n="${num}"]`);
        },

        firstword(num,row) {
            const el = row ? row : _state.xml;
            return el.querySelector(`w[n="${num}"]`);
        },

        normal(el) {
            const par = el ? el : document.getElementById('views');
            //const par = el ? el : _state.matrix.boxdiv;
            return par.querySelectorAll('.lemma[data-normal], .tree-lemma[data-normal]');
        },

        htmlreading(el, normalized) {
            return normalized && el.dataset.normal ?
                el.dataset.normal :
                el.IAST.textContent;
        },
    /*
        xmlreading(label,n,normalized) {
            const el = _state.xml.querySelector(`TEI[n="${label}"] > text > w[n="${n}"]`);
            return normalized && el.hasAttribute('lemma') ?
                el.getAttribute('lemma') :
                el.textContent;
        },

        xmlreadings(label, normalized) {
            const els = [..._state.xml.querySelectorAll(`TEI[n="${label}"] > text > w`)];

            return els.map(el => {
                normalized && el.hasAttribute('lemma') ?
                    el.getAttribute('lemma') :
                    el.textContent;
            });
        },
    */
        ths() {
            return _state.matrix.boxdiv.querySelectorAll('th[data-ref]');
        },

        firstth(num) {
            return _state.matrix.boxdiv.querySelector(`th[data-ref="${num}"]`);
        },
        checkbox(num,type) {
            return _state.matrix.boxdiv.querySelector(`th[data-ref="${num}"] input.${type}`);
        },

        highlit() {
            const firstrow = find.firsttr();
            const lemmata = firstrow.querySelectorAll('.highlit');
            //if(lemmata.length === 0) return false;
            const nums = new Set();
            for(const lemma of lemmata) {
                nums.add(lemma.dataset.n);
            }
            return nums;
        },

        highlitcell() {
            return _state.matrix.boxdiv.querySelector('td.highlitcell');
        },

        highlitrow() {
            const highlitcell = find.highlitcell();
            return highlitcell ? highlitcell.closest('tr') : false;
        },

        lowhigh(nums) {
            const sortednums = [...nums].sort((a,b) => parseInt(a)-parseInt(b));
            const low = parseInt(sortednums[0]);
            const high = sortednums.length > 1 ?
                parseInt(sortednums[sortednums.length-1]) :
                undefined;
            return [low,high];
        },

        readings(num, element) {
            const el = element ? element : _state.matrix.boxdiv;
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

        attr(type,nums) {
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

        whichattr(el) {
            if(el.hasAttribute('n')) return 'n';
            else if(el.hasAttribute('data-n')) return 'data-n';
            else if(el.hasAttribute('data-ref')) return 'data-ref';
            else return false;
        },

        clauses(nums,strict = false) {
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
        },

        clausesToRemove(clgroups,nums,threshold = 0) {
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

        empty() {
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
        
        emptyRows(labels = false) {
            const trs = find.trs();
            const emptyrows = new Set();
            for(let n=0;n<trs.length;n++) {
                const tds = find.tds(false,trs[n]);
                const emptyrow = (() => {
                    for(const td of tds)
                        if(td.textContent !== '')
                            return false;
                    return true;
                })();
                if(emptyrow)
                    labels ? emptyrows.add(trs[n].dataset.n) : emptyrows.add(n); 
            }
            return emptyrows;
        },

        prevNonempty(index,arr) {
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

        nextNonempty(index,arr) {
            for(let n=index;n<arr.length;n++) {
                const td = arr[n];
                if(td.textContent !== '') return n;
            }
            return false;
        },
        /*
        setIntersection(...sets) {
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

        setUnion(...sets) {
            return new Set(
                sets.reduce((acc, cur) => {
                    acc = [...acc,...cur];
                    return acc;
                },[])
            );
        },
        */
        cursorPos(el) {
            const range = window.getSelection().getRangeAt(0);
            const preCaretRange = range.cloneRange();
            preCaretRange.selectNodeContents(el);
            const fullLength = preCaretRange.toString().length;
            preCaretRange.setEnd(range.endContainer, range.endOffset);
            const caretOffset = preCaretRange.toString().length;
            return [caretOffset,fullLength];
        },

        selection() {    
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
        },
    }; // end find

    const check = {
        undo() {
            return _state.undo.length > 0 ? true : false;
        },

        redo() {
            return _state.redo.length > 0 ? true : false;
        },

        checkbox(type,nums = null) {
            if(!check.anyhighlit()) return false;

            const numss = nums === null ?
                find.highlit() :
                nums;

            const states = find.attr(type,numss);
            for(const state of states.values())
                if(state === false)
                    return false;
            return true;
        },

        grouped() {
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

        oneGrouped() {
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

        anyhighlit() {
            return _state.matrix.boxdiv.querySelector('td.highlit') ? true : false;
        },

        manyhighlit() {
            return find.highlit().size > 1;
        },

        highlitcell() {
            return _state.matrix.boxdiv.querySelector('td.highlitcell') ? true : false;
        },

        normalizedView() {
            return document.getElementById('views').classList.contains('normalized');
        },

        anyNormalized() {
            return _state.matrix.boxdiv.querySelector('.lemma[data-normal]');
        },

        headerView() {
            return _state.matrix.boxdiv.querySelector('tr.header').style.display === 'none' ? false : true;
        },
    }; // end check

    const make = {
        tei: function(label) {
            const tei = _state.xml.createElementNS(_state.teins,'TEI');
            tei.setAttribute('n',label);
            /*
            const text = _state.xml.createElementNS(_state.teins,'text');
            tei.appendChild(text);
            make.emptywords(text);
            */
            const template = find.firsttext().cloneNode(true);
            tei.appendChild(template);
            for(const w of find.words(false,template)) {
                w.removeAttribute('lemma');
                while(w.firstChild)
                    w.firstChild.remove();
            }
            return tei;
        },
        
        xmlel: function(name,doc = _state.xml) {
            return doc.createElementNS(_state.teins,name);
        },

        emptycell: function(n) {
            const td = document.createElement('td');
            td.className = 'lemma';
            td.dataset.n = n;
            return td;
        },

        emptyword: function(n,doc = _state.xml) {
            const w = doc.createElementNS(_state.teins,'w');
            w.setAttribute('n',n);
            return w;
        },

        emptywords: function(text,max,start) {
            const m = max || _state.maxlemma;
            const n_start = start || 0;
            for(let n = n_start; n <= m; n++) {
                const word = make.emptyword(n);
                text.appendChild(word);
            }
            

        },
        row: function(label,type) {
            const tr = document.createElement('tr');
            const th = document.createElement('th');
            th.scope = 'row';
            th.draggable = true;
            th.appendChild(document.createTextNode(label));
            //th.addEventListener('dragstart',events.thDragStart);
            tr.dataset.n = label;
            tr.appendChild(th);
            const firstrow = find.firsttr();
            for(const ftd of firstrow.querySelectorAll('td')) {
                const td = document.createElement('td');
                td.dataset.n = ftd.dataset.n;
                td.className = ftd.className;
                if(type) td.classList.add(type);
                tr.appendChild(td);
            }
            /*
            for(let n=0;n<=_state.maxlemma;n++) {
                const td = document.createElement('td');
                td.dataset.n = n;
                td.className = 'lemma';
                if(type) td.classList.add(type);
                tr.appendChild(td);
            }
            */
            return tr;
        },
        blackout(frag,func) {
            const blackout = document.createElement('div');
            blackout.id = 'blackout';
            blackout.appendChild(frag);
            document.body.appendChild(blackout);
            
            const submitFunc = function(e) {
                func(e);
                blackout.parentNode.removeChild(blackout);
            };

            const blackoutClick = function(e) {
                const targ = e.target.closest('.popup');
                if(!targ) {
                    const blackout = document.querySelector('#blackout');
                    blackout.parentNode.removeChild(blackout);
                }
            }

            const submit = blackout.querySelector('button');
            submit.addEventListener('click',submitFunc);
            blackout.addEventListener('click',blackoutClick);
            //return blackout;
        },

    }; // end make

    return {
        find: find,
        check: check,
        make: make
    };
};

export { Utils };
