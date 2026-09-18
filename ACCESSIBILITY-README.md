# Accessible dissertation build

This working copy is prepared for the native LaTeX tagged-PDF workflow in
TeX Live 2026 or newer.

## Changes made

- Enabled PDF/UA-2 metadata with `\DocumentMetadata` before `\documentclass`.
- Declared the document language as US English and added title, author,
  subject, and keyword metadata.
- Added content-specific alternative text to all 28 active graphics.
- Declared the first row as the header row in all six tables.
- Corrected the two malformed table captions in `chapter2.tex`.
- Replaced seven duplicate `fig:one` labels with unique labels.
- Removed the duplicate `geometry` package load.
- Removed the legacy `physics` package dependency and supplied the two matrix
  element commands used by the dissertation.
- Replaced three `pdfpages` insertions with selectable text transcripts. The
  original PDFs remain available for comparison.

## Required missing file

Place the original `dissertation.bib` file in this directory before building.
It was not included with the supplied project. Without it, the references and
numeric citations cannot be generated correctly.

## Build

Use TeX Live 2026 or newer. From this directory run:

```sh
pdflatex dissertation.tex
bibtex dissertation
pdflatex dissertation.tex
pdflatex dissertation.tex
```

The source uses `biblatex` with the BibTeX backend, so the command is `bibtex`,
not `biber`.

## Validation

Run:

```sh
pdfinfo dissertation.pdf
python3 validate_accessibility.py dissertation.pdf
```

The final PDF must report `Tagged: yes`; contain `/StructTreeRoot`, `/MarkInfo`,
and `/Lang`; and show nonempty title and author metadata. A PDF/UA checker such
as PAC or veraPDF should then be used for the formal conformance report.

Review the generated table of contents, lists, appendix code transcripts, and
CV visually before submission. The text transcripts were mechanically
extracted from the original notebook and CV PDFs to avoid inaccessible imported
page images; compare them against the retained originals for transcription
accuracy.
