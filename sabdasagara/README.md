# sabdasagara

Splits a set of Sanskrit texts (in FASTA format) into ngrams, with normalization and optional fuzzy matching.

Syntax:

    stack exec -- sabdasagara-exe -n 2 -f input.fas > output.csv

This will split the texts in input.fas into 2grams, with fuzzy matching. For more options, run

    stack exec -- sabdasagara-exe

