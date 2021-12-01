# Scoring matrix for aligning Sanskrit texts

The file `substitution_matrix.ods` is a LibreOffice spreadsheet with a basic scoring matrix. It can be used with helayo or with MAFFT, when performing a multiple sequence alignment of Sanskrit texts. For MAFFT, texts should be in SLP1 format (with some substitutions; see the scheme in `Transcribe.hs`). To use the scoring matrix:

* Export the spreadsheet to CSV
* convert the CSV file to the format used by MAFFT
  * e.g., `csv2mafft substitution_matrix.csv > substitution_matrix_mafft`
* align your sequences with the scoring matrix
  * e.g., `mafft --text-matrix substitution_matrix_mafft unaligned.fas > aligned.fas`

Scoring scheme:
    
* match: 1
* mismatch between a vowel and a vowel: -1
* mismatch between a consonant and a consonant: -1
* mismatch between a vowel and a consonant: -1.25

Effectively, it tries to align vowels with vowels and consonants with consonants. Anusvāra and visarga are treated as a third category.

| No scoring matrix  | With scoring matrix |
| ------------------ | ------------------- |
| `viśayaḥyadeva`    | `viśayaḥyadeva`     |
| `viśayaḥya----`    | `viśayaḥya----`     |
| `viśayaḥya---t`    | `viśayo-yat---`     |
| `viśay-oya---ṃ `   | `viśayo-yaṃ---`     |
| `viśay-oya----`    | `viśayo-ya----`     |
