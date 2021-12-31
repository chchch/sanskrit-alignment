
const check = function (_state, Find) {
    const _this = this;
    this.undo = function() {
        return _state.undo.length > 0 ? true : false;
    };

    this.redo = function() {
        return _state.redo.length > 0 ? true : false;
    };

    this.checkbox = function(type,nums) {
        if(!_this.anyhighlit()) return false;

        const numss = nums === false ?
            Find.highlit() :
            nums;

        const states = Find.attr(type,numss);
        for(const state of states.values())
            if(state === false)
                return false;
        return true;
    };

    this.grouped = function() {
        const nums = Find.highlit();
        if(!nums) return false;
        const firstrow = Find.firsttr();
        for(const num of nums) {
            const cell = Find.firsttd(num,firstrow);
            if(cell.classList.contains('group-start') ||
           cell.classList.contains('group-internal') ||
           cell.classList.contains('group-end'))
                return true;
        }
        return false;
    };

    this.oneGrouped = function() {
        const nums = Find.highlit();
        if(nums.size === 0) return false;
        if(nums.size === 1) {
            const firstrow = Find.firsttr();
            const cell = Find.firsttd([...nums][0],firstrow);
            //const cell = firstrow.querySelector('td[data-n="'+[...nums][0]+'"]');
            if(cell.classList.contains('group-start') ||
           cell.classList.contains('group-end') ||
           cell.classList.contains('group-internal'))
                return true;
            else
                return false;
        }
        return true;
    };

    this.anyhighlit = function() {
        return _state.matrix.boxdiv.querySelector('td.highlit') ? true : false;
    };

    this.manyhighlit = function() {
        return Find.highlit().size > 1;
    };

    this.highlitcell = function() {
        return _state.matrix.boxdiv.querySelector('td.highlitcell') ? true : false;
    };

    this.normalizedView = function() {
        return document.getElementById('views').classList.contains('normalized');
    };

    this.anyNormalized = function() {
        return _state.matrix.boxdiv.querySelector('.lemma[data-normal]');
    };

    this.headerView = function() {
        return _state.matrix.boxdiv.querySelector('tr.header').style.display === 'none' ? false : true;
    };
};

export { check };
