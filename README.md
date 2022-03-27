# Tools for aligning Sanskrit texts

[![DOI](https://joss.theoj.org/papers/10.21105/joss.04022/status.svg)](https://doi.org/10.21105/joss.04022)
![helayo build & test](https://github.com/chchch/sanskrit-alignment/actions/workflows/build.yml/badge.svg)
![matrix-editor test](https://github.com/chchch/sanskrit-alignment/actions/workflows/jstest.yml/badge.svg)

This repository contains _helayo_, a program for aligning/collating Sanskrit texts, and _matrix-editor_, a web-based interface for editing those alignments and producing tree-based textual reconstructions.

See the docs at https://chchch.github.io/sanskrit-alignment/docs/ for more information.

## directories

* csv2mafft/ — tools for working with MAFFT
* docs/ — the location of the tutorial
* example/ — the example files used in the tutorial
    * example/fastt/ — FASTT files exported from saktumiva.org
    * example/xml/ — those files aligned, which can be opened in the matrix-editor
    * example/trees/ ― phylogenetic trees produced from the alignments
* helayo/ — the alignment program
    * helayo/dist/ ― binaries for MacOS, Ubuntu Linux, and Windows
* matrix-editor/ — an interface to view/edit alignments, export them to be used with phylogenetic tree-building software, and reconstruct texts based on those trees; this can accessed [online](https://chchch.github.io/sanskrit-alignment/matrix-editor)

This repository has been published in the Journal for Open Source Software:

Li, C., (2022). helayo: Reconstructing Sanskrit texts from manuscript witnesses. Journal of Open Source Software, 7(71), 4022, https://doi.org/10.21105/joss.04022

