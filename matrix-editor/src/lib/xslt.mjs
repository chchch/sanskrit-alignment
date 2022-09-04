import csvXSLT from '../xslt/csv.xsl?raw';
import prettyXSLT from '../xslt/prettyprint.xsl?raw';
import matrixXSLT from '../xslt/matrix.xsl?raw';
import treeXSLT from '../xslt/tree.xsl?raw';
import lemmaXSLT from '../xslt/lemma.xsl?raw';
import lgXSLT from '../xslt/lg.xsl?raw';

const makeProc = function(sheet) {
    const parser = new DOMParser();
    const xslsheet = parser.parseFromString(sheet,'text/xml');
    const xslt_proc = new XSLTProcessor();
    xslt_proc.importStylesheet(xslsheet);
    return xslt_proc;
};

const xslt = function(_state) {
    
    this.transformString = function(s,proc) {
        const temp = _state.xml.createElementNS(_state.teins,'ab');
        temp.innerHTML = s;
        //temp.setAttribute('xmlns','http://www.w3.org/1999/xhtml');
        return proc.transformToFragment(temp,document);
    };

    this.sheets = {
        csv: makeProc(csvXSLT),
        xml: makeProc(prettyXSLT),
        matrix: makeProc(matrixXSLT),
        lemma: makeProc(lemmaXSLT),
        lg: makeProc(lgXSLT),
        tree: makeProc(treeXSLT)
    }
};

export { xslt };
