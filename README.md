# Tools for aligning Sanskrit texts

![helayo build & test](https://github.com/chchch/sanskrit-alignment/actions/workflows/build.yml/badge.svg)

This repository contains some work-in-progress centered on aligning Sanskrit texts (for doing collation, reconstruction, etc.)

See the docs at https://chchch.github.io/sanskrit-alignment/docs/index.html for more information.

## directories

* csv2mafft/ — tools for working with MAFFT (not covered here)
* docs/ — the location of this tutorial
* example/ — containing the example files used in this tutorial
    * example/fasta/ — FASTA files exported from saktumiva.org
    * example/xml/ — those files aligned, which can be opened in the matrix-editor
* helayo/ — the alignment tool (see Alignment)
* matrix-editor/ — an interface to view/edit alignments, export them to be used with phylogenetic tree-building software, and reconstruct texts based on those trees 
