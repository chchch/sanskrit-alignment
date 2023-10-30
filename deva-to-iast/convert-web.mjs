import { convertFiles } from './common.mjs';
import { showSaveFilePicker } from 'https://cdn.jsdelivr.net/npm/native-file-system-adapter/mod.js';

const _selectedfiles = new Map();

const parseString = (str) => {
    const parser = new DOMParser();
    const newd = parser.parseFromString(str,'text/xml');
    if(newd.documentElement.nodeName === 'parsererror')
        alert(`The XML file could not be loaded. Please contact your friendly local system administrator. Error: ${newd.documentElement.textContent}`);
    else
        return newd;
};

const upload = async (arr) => {
    const files = arr.map(file => {
        return readOne(file);
    });
    return await Promise.all(files);
};

const readOne = async (file) => {
    const reader = new FileReader();
    return new Promise(res => {
        reader.onload = () => res(reader.result);
        reader.readAsText(file);
    });
};

const updatePreview = async () => {
    const preview = document.getElementById('preview');
    const input = document.getElementById('teifiles');

    const files = [...input.files];
    if(files.length === 0) {
        const p = document.createElement('p');
        p.textContent = 'No files selected.';
        preview.appendChild(p);
    }
    else {
        appendList(preview, files.map(f => f.name));  
        
        //const texts = await upload(files);
        //for(const text of texts) {
        for(const file of files) {
            const text = await readOne(file);
            const teixml = parseString(text);
            _selectedfiles.set(file.name,teixml);
        }

        document.getElementById('convertfile').disabled = false;
    }
};

const appendList = (par, els) => {
    while (par.firstChild) par.firstChild.remove();
    const list = document.createElement('ul');
    for(const el of els) {
        const item = document.createElement('li');
        item.textContent = el;
        list.appendChild(item);
    }
    par.appendChild(list);
};

const main = async () => {
    const outtexts = convertFiles([..._selectedfiles.values()]);
    console.log(outtexts);
    const filenames = [..._selectedfiles.keys()];

    const zip = new JSZip();
    for(let n=0;n<outtexts.length;n++) 
        zip.file(filenames[n], new XMLSerializer().serializeToString(outtexts[n]));
    zip.generateAsync({type: "blob"})
       .then(async (blob) => {
            //const file = new Blob([xml.serialize(newdoc)], {type: 'text/xml;charset=utf-u'});
            //const filename = eadfile.name.replace(/^\[(.+)\]$/,'$1_TST.xml');
            const fileHandle = await showSaveFilePicker({
                _preferPolyfill: false,
                suggestedName: 'converted.zip',
                types: [ {description: 'Zip archive', accept: {'application/zip': ['.zip']} } ],
            });
            const writer = await fileHandle.createWritable();
            writer.write(blob);
            writer.close();
       });
};

window.addEventListener('load', () => {
    document.getElementById('teifiles').addEventListener('change',updatePreview);
    document.getElementById('convertfile').addEventListener('click',main);
});
