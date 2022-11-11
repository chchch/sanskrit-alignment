class Filter {
    static Hide = Symbol('hide');
    static Ignore = Symbol('ignore');
    static IgnoreTag = Symbol('ignoretag');
    static Show = Symbol('show');

    constructor(name) {
        this.name = name;
    }
};

const Tags = new Map([
            // analysis
           ['pc', Filter.IgnoreTag],
            // core
           ['abbr', Filter.Ignore],
           ['add', Filter.IgnoreTag],
           ['cb', Filter.Ignore],
           ['choice', Filter.IgnoreTag],
           ['corr', Filter.Ignore],
           ['del', Filter.Ignore],
           ['emph', Filter.IgnoreTag],
           ['ex', Filter.Ignore],
           ['expan', Filter.IgnoreTag],
           ['foreign', Filter.IgnoreTag],
           ['gap', Filter.Ignore],
           ['head', Filter.IgnoreTag],
           ['hi', Filter.IgnoreTag],
           ['l', Filter.IgnoreTag],
           ['label', Filter.Ignore],
           ['lb', Filter.Ignore],
           ['lg', Filter.IgnoreTag],
           ['milestone', Filter.Ignore],
           ['note', Filter.Ignore],
           ['num', Filter.Ignore],
           ['pb', Filter.Ignore],
           ['ptr', Filter.Ignore],
           ['unclear', Filter.IgnoreTag],
            // gaiji
           ['g', Filter.IgnoreTag],
            // linking
           ['anchor', Filter.Ignore],
            // msdescription
           ['locus', Filter.Ignore],
            // transcr
           ['metamark', Filter.Ignore],
           ['orig', Filter.IgnoreTag],
           ['retrace', Filter.IgnoreTag],
           ['sic', Filter.IgnoreTag],
           ['space', Filter.Ignore],
           ['subst', Filter.IgnoreTag],
           ['supplied', Filter.IgnoreTag],
           ['surplus', Filter.Ignore],
            // verse
           ['caesura', Filter.Ignore],
            // TEI Title
           ['sub', Filter.IgnoreTag],
           ['sup', Filter.IgnoreTag],
            // textcrit
           ['app', Filter.IgnoreTag],
           ['lem', Filter.IgnoreTag],
           ['rdg', Filter.Ignore],
]);
            // XML
  //          '#comment': Filter.Ignore,

export { Filter, Tags };
