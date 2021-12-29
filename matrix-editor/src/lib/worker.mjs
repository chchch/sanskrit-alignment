const workerFunc = function() {
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
            const val = mss.get(taxon);
            //const reading = mss.get(taxon)[n];
            //const reading = val? val[n] : '';
            const reading = val ? [val[n]] : [];
            firstpass.set(taxon,new Set(reading));
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
        
        /*
        for(const [node,children] of levels[levels.length-1]) {
            secondpass.set(node,firstpass.get(node));
        }
        */
        for(const level of levels[levels.length-1])
            secondpass.set(level[0],firstpass.get(level[0]));

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
};

export { workerFunc };
