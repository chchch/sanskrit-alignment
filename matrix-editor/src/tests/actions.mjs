import { _state } from './loader.mjs';
import { Utils as _Utils } from '../lib/utils.mjs';
import {actions as _Actions } from '../lib/actions.mjs';

const Utils = new _Utils(_state);
const Actions = new _Actions(Utils);

const getRand = (min,max) => {
    return Math.floor(Math.random() * (max - min)) + min;
}

const test = {
    
    groups() {
        const oldxml = _state.xml.documentElement.innerHTML;
        const oldhtml = _state.matrix.boxdiv.documentElement.innerHTML;

        const max = getRand(2,Utils.find.maxlemma());
        const min = getRand(0,max-1);
        const range = new Set([...Array(max-min).keys()].map(n => n+min));

        console.log(`Grouping/ungrouping columns ${min} to ${max}:`);

        Actions.group(range);
        Actions.ungroup(range);

        if(oldxml === _state.xml.documentElement.innerHTML)
            console.log('XML identical.');
        else
            throw('XML error!');

        if(oldhtml === _state.matrix.boxdiv.documentElement.innerHTML)
            console.log('HTML identical.');
        else
            throw('HTML error!');
    },
};

test.groups();
