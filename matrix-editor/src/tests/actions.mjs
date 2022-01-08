import { _state } from './loader.mjs';
import { Utils as _Utils } from '../lib/utils.mjs';
import {actions as _Actions } from '../lib/actions.mjs';

const Utils = new _Utils(_state);
const Actions = new _Actions(Utils);

const test = {
    
    groups() {
        const oldxml = _state.xml.documentElement.innerHTML;
        const oldhtml = _state.matrix.boxdiv.documentElement.innerHTML;

        const max = Math.floor(Math.random() * Utils.find.maxlemma());
        const min = max - Math.floor(Math.random() * (max-1));
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
