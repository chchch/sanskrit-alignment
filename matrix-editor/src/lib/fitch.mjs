// Tree labeling via the Fitch algorithm

const Fitch = function(mss,levels) {

    const set = {
        Intersection: function(...sets) {
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

        Union: function(...sets) {
            return new Set(
                sets.reduce((acc, cur) => {
                    acc = [...acc,...cur];
                    return acc;
                },[])
            );
        },
    };

    const fitch1 = function(target) {
        const firstpass = new Map();
        for(const taxon of levels[0]) {
            const val = mss.get(taxon);
            const reading = val !== undefined ? [val] : [];
            firstpass.set(taxon,new Set(reading));
        }
        for(let m=1;m<levels.length;m++) { // start at 1 (after taxa)
            for(const [node,children] of levels[m]) {
                const readings = children.map(node => firstpass.get(node));
                const intersection = set.Intersection(...readings);
            
                // shortcut, if we only care about one node
                if(target && node === target && intersection.size === 1)
                    return [...intersection][0];

                const result = intersection.size > 0 ?
                    intersection :
                    set.Union(...readings);
                firstpass.set(node,result);

            }
        }
        return firstpass;

    };

    const fitch2 = function(firstpass,target) {
        const taxa = levels[0];
        const secondpass = new Map();
        
        for(const level of levels[levels.length-1])
            secondpass.set(level[0],firstpass.get(level[0]));

        for(let n=levels.length-1;n>1;n--) {
            for(const [node,children] of levels[n]) {
                const ancestral = secondpass.get(node);
                for(const child of children) {
                    if(taxa.indexOf(child) !== -1)
                        continue;
                    const childreading = firstpass.get(child);
                    const result = (() => {
                        if(childreading.size === 1)
                            return childreading;

                        const intersection = set.Intersection(ancestral,childreading);
                        return intersection.size > 0 ?
                            intersection :
                            childreading;
                    })();
                    
                    // if reading of target node found, skip rest of reconstruction
                    if(target && child === target)
                        return result;

                    secondpass.set(child,result);
                }
            }
        }
        return target ? secondpass.get(target) : secondpass;
    };

    const fitch = function(target = null) {
        const firstpass = fitch1(target);
        if(typeof firstpass === 'string') {
            return firstpass;
        }
    
        // do the second pass if the first pass is inconclusive
        const secondpass = fitch2(firstpass,target);
        return target ? formatOutput(secondpass) :
            new Map(Array.from(secondpass,([key,val]) => [key,formatOutput(val,true)]));
    };

    const formatOutput = function(m,blanks = false) {
        if(!blanks && m.size === 1) return [...m][0].trim();

        const output = [...m].map(str => str.trim() === '' ? '_' : str);
        return output.length === 1 ? output[0] : '{' + output.join(', ') + '}';
    };

    return {
        run: fitch
    }
};

export { Fitch };

