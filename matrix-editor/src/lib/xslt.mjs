import csvXSLT from '../xslt/csv.xsl';
import prettyXSLT from '../xslt/prettyprint.xsl';
import matrixXSLT from '../xslt/matrix.xsl';
import treeXSLT from '../xslt/tree.xsl';
import lemmaXSLT from '../xslt/lemma.xsl';
import lgXSLT from '../xslt/lg.xsl';

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
