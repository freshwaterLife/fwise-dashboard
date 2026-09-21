// typst-show.typ
// Applies the FWISE page frame (fwise-report() in typst-template.typ) to the
// whole document. Replaces Quarto's own article() show rule, which would set
// its own page, title block and fonts over ours.
#show: fwise-report
