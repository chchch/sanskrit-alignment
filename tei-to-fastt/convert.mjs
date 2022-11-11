import yargs from 'yargs';
import fs from 'fs';
import jsdom from 'jsdom';
import path from 'path';
import { hideBin } from 'yargs/helpers';
import { convertFiles } from './common.mjs';

const argv = yargs(hideBin(process.argv))
    .option('files', {
        alias: 'f',
        description: 'Input files',
        type: 'string'
    })
    .option('ids', {
        alias: 'i',
        description: 'XML ids',
        type: 'string'
    })
    .option('out', {
        alias: 'o',
        description: 'Output directory',
        type: 'string'
    })
    .help().alias('help','h').argv;

const parseXML = function(str) {
    const dom = new jsdom.JSDOM('');
    const parser = new dom.window.DOMParser();
    return parser.parseFromString(str,'text/xml');
};

const main = function() {
    const infiles = argv.f;
    if(!infiles) { console.error('No input files'); return; }
    
    const intexts = infiles.split(' ').map(infile => parseXML(fs.readFileSync(infile,{encoding: 'utf-8'})));
    
    const ids = argv.i?.split(' ') || [...intexts[0].querySelectorAll('p[xml:id],lg[xml:id],l[xml:id]')]
                                    .map(id => id.getAttribute('xml:id'));
    
    const outtexts = convertFiles(intexts, ids);
    const outdir = argv.o ? argv.o.replace(/\/$/,'') : '.';
    if(outdir !== '.' && !fs.existsSync(outdir)) fs.mkdirSync(outdir, {recursive: true});

    for(const outtext of outtexts) {
        fs.writeFile(`${outdir}/${outtext.id}.fas`,outtext.text,{encoding: 'utf-8'},function(){return;});
    }
};

main();
