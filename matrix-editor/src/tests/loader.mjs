import Fs from 'fs';
import Jsdom from 'jsdom';
import SaxonJS from 'saxon-js';

const _state = {
    matrix: {},
    textboxes: []
};

const xsltSheet = Fs.readFileSync('tests/matrix.sef.json',{encoding:'utf-8'});

const dir = '../../example/xml/';
const xmlFiles = (() => {
    const files = Fs.readdirSync(dir,{encoding:'utf-8'});
    const flist = [];
    files.forEach(f => {
        if(/^[^_].+\.xml$/.test(f))
            flist.push(dir+f);
    });
    return flist;
})();
const filename = xmlFiles[Math.floor(Math.random() * xmlFiles.length)];

console.log(`Loading ${filename}.`);

const xmlFile = Fs.readFileSync(filename,{encoding:'utf-8'});

const processed = SaxonJS.transform({
    stylesheetText: xsltSheet,
    sourceText: xmlFile,
    destination: 'serialized'},'sync');

const dom = new Jsdom.JSDOM('');
const parser = new dom.window.DOMParser();

_state.xml = parser.parseFromString(xmlFile,'text/xml');
_state.matrix.boxdiv = parser.parseFromString(`<!DOCTYPE html>${processed.principalResult}`,'text/html');

export { _state };
