const actions = function(Utils) {
    const { find: Find, check: Check, make: Make } = Utils;

    const group = function(nums) {
        const numarr = [...nums];
        const firstnum = numarr.shift();
    
        const texts = Find.texts();
        for(const text of texts) {
            const cl = Make.xmlel('cl');
            const firstw = Find.firstword(firstnum,text);
            firstw.parentNode.insertBefore(cl,firstw);
            cl.appendChild(firstw);
            for(const num of nums)
                cl.appendChild(Find.firstword(num,text));
        }

        const lastnum = numarr.pop();
    
        for(const td of Find.tds(firstnum)) {
            td.classList.add('group-start');
        }
        for(const td of Find.tds(lastnum)) {
            td.classList.add('group-end');
        }
        for(const num of numarr) {
            for(const td of Find.tds(num)) {
                td.classList.add('group-internal');
            }
        }
    };

    const ungroup = function(nums) {
        const texts = Find.texts();

        // ungroup xml
        for(const text of texts) {
            let cl;
            for(const num of nums) {
                const word = Find.firstword(num,text);
                if(!cl) cl = word.closest('cl');
                cl.parentNode.insertBefore(word,cl);
            }
            cl.parentNode.removeChild(cl);
        }
    
        // ungroup html
        const tds = [...nums].flatMap(n => [...Find.tds(n)]);
        for(const td of tds)
            td.classList.remove('group-start','group-internal','group-end');
    };

    return {
        group: group,
        ungroup: ungroup
    };
};

export { actions };
