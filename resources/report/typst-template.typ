// typst-template.typ
// The PDF report's page frame and components (a Quarto template partial).
// No values here: everything comes from fwise-tokens.typ, written by
// fw_pdf_tokens(). R calls these functions with every text as an escaped
// string literal (fw_typ_str()), never as markup.

#import "fwise-tokens.typ": *

// ---- The page ----------------------------------------------------------------

// Every page: the tinted ground (no white) and a footer of the site's logos
// (FWISE first), with the page count on its own line below.
#let fw-footer = context {
  line(length: 100%, stroke: 0.5pt + fw-border)
  v(2mm)
  // Two rows of four, each logo centred in an equal column.
  grid(
    columns: (1fr, 1fr, 1fr, 1fr),
    row-gutter: 4mm,
    column-gutter: 6mm,
    align: center + horizon,
    // Fit to the cell, so a wide logo shrinks rather than overlapping.
    ..(fw-logo-mark, ..fw-logos).map(p => image(p, width: 100%, height: fw-logo-h, fit: "contain")),
  )
  v(1.5mm)
  align(right, text(size: fw-credit, fill: fw-ink-muted)[
    #fw-page-label #counter(page).display() #fw-of-label #counter(page).final().first()
  ])
}

#let fwise-report(doc) = {
  set document(title: fw-doc-title)
  set page(
    paper: "a4",
    fill: fw-page-fill,
    // The bottom margin reserves the footer; too small and text overlaps it.
    margin: (x: fw-margin, top: fw-margin, bottom: fw-margin + 32mm),
    footer: fw-footer,
    footer-descent: 5mm,
  )
  set text(font: "Ubuntu", size: fw-floor, fill: fw-ink, lang: "en")
  set par(leading: fw-leading, justify: false, spacing: 1.1em)
  set table(stroke: none)
  show link: set text(fill: fw-teal-text)
  show heading.where(level: 1): it => block(
    above: 0pt, below: 3mm,
    text(size: fw-title, weight: "bold", fill: fw-ink, it.body),
  )
  show heading.where(level: 2): it => block(
    above: 9mm, below: 2.5mm, sticky: true,
    text(size: fw-h2, weight: "bold", fill: fw-ink, it.body),
  )
  show heading.where(level: 3): it => block(
    above: 5mm, below: 2mm, sticky: true,
    text(size: fw-h3, weight: "bold", fill: fw-ink, it.body),
  )
  doc
}

// ---- The letterhead ----------------------------------------------------------

// The explicit lockup at fw-mark-h, larger than the footer logos (client).
// The v(2.5mm) adds space between title and subtitle (client).
#let fw-letterhead(title: "", subtitle: "") = {
  image(fw-logo-explicit, height: fw-mark-h)
  v(7mm)
  heading(level: 1)[#title]
  v(2.5mm)
  text(fill: fw-ink-muted)[#subtitle]
  v(3mm)
  line(length: 100%, stroke: 2pt + fw-teal)
}

// ---- Blocks ------------------------------------------------------------------

// A titled block, the note under the heading and above the figure. Both
// sticky, so a page never ends on a title.
#let fw-block(title: "", note: none, body) = {
  heading(level: 2)[#title]
  if note != none {
    block(below: 3mm, sticky: true, text(fill: fw-ink-muted)[#note])
  }
  body
}

#let fw-caption(txt) = block(above: 2mm, text(fill: fw-ink-muted)[#txt])

#let fw-figure(path, height) = block(
  breakable: false, above: 2mm, below: 2mm,
  image(path, width: 100%, height: height),
)

// ---- The summary strip -------------------------------------------------------

// The five headline numbers, each label under its number.
#let fw-stats(items) = block(
  fill: fw-teal-tint, radius: 3mm, inset: (x: 5mm, y: 4mm), width: 100%,
  breakable: false,
  grid(
    columns: (1fr,) * items.len(),
    column-gutter: 4mm,
    ..items.map(it => stack(spacing: 1.5mm,
      text(font: "Ubuntu Mono", size: fw-stat, weight: "bold", fill: fw-teal-text)[#it.at(0)],
      text(fill: fw-ink)[#it.at(1)],
    )),
  ),
)

// ---- Outcome bars ------------------------------------------------------------

// All four outcomes; label and count are text, the bar is decoration.
#let fw-outcome-bars(rows) = block(
  breakable: false,
  grid(
    columns: (32mm, 1fr, 30mm),
    column-gutter: 4mm, row-gutter: 3mm,
    align: (left + horizon, left + horizon, right + horizon),
    ..rows.map(r => (
      text[#r.at(0)],
      box(width: 100%, height: 5mm, fill: fw-sunken, radius: 1mm,
        if r.at(2) > 0 {
          box(width: r.at(2) * 1%, height: 5mm, fill: rgb(r.at(3)), radius: 1mm)
        }),
      text(font: "Ubuntu Mono")[#r.at(1)],
    )).flatten(),
  ),
)

// ---- Species tiles -----------------------------------------------------------

// One row of tiles: photo, name, count, success-rate bar, and the photo's
// credit and licence (a licence condition).
#let fw-species-tiles(tiles) = block(
  breakable: false,
  grid(
    columns: (1fr,) * tiles.len(),
    column-gutter: 5mm,
    ..tiles.map(t => {
      if t.image != none {
        block(clip: true, radius: 2mm, width: 100%, height: 38mm,
          image(t.image, width: 100%, height: 38mm, fit: "cover"))
      } else {
        block(fill: fw-sunken, radius: 2mm, width: 100%, height: 38mm,
          align(center + horizon, text(fill: fw-ink-muted)[#fw-no-image]))
      }
      if t.credit != none {
        block(above: 1mm, text(size: fw-credit, fill: fw-ink-muted)[
          #if t.credit_url != none { link(t.credit_url)[#t.credit] } else { t.credit }
          #fw-credit-sep
          #if t.licence_url != none { link(t.licence_url)[#t.licence] } else { t.licence }
        ])
      }
      block(above: 2mm, below: 1.5mm, text(weight: "bold")[#t.name])
      block(above: 0mm, text(fill: fw-ink-muted)[#t.count])
      block(above: 2mm, width: 100%, height: 3.5mm, radius: 1mm, clip: true,
        stack(dir: ltr, ..t.segments.map(s =>
          box(width: s.at(0) * 1%, height: 3.5mm, fill: rgb(s.at(1)))))
      )
    }),
  ),
)

// ---- Tables ------------------------------------------------------------------

// The app's table style: repeating sunken header, hairlines, mono figures.
// `size` is only passed by the "What this report covers" table (fw-small).
#let fw-table(headers, rows, num: (), widths: auto, size: none) = {
  let n = headers.len()
  let cols = if widths == auto { (auto,) * (n - 1) + (1fr,) } else { widths }
  set text(size: size) if size != none
  table(
    columns: cols,
    inset: (x: 2.5mm, y: 2mm),
    align: (col, row) => if col in num { right + top } else { left + top },
    fill: (col, row) => if row == 0 { fw-sunken } else { none },
    stroke: (col, row) => (bottom: 0.5pt + fw-border),
    table.header(..headers.map(h => text(weight: "bold")[#h])),
    ..rows.map(r => r.enumerate().map(((i, v)) =>
      if i in num { text(font: "Ubuntu Mono")[#v] } else { [#v] }
    )).flatten(),
  )
}

// ---- Caveats -----------------------------------------------------------------

#let fw-caveats(heading-text, blocks) = {
  heading(level: 2)[#heading-text]
  block(
    fill: fw-teal-wash, radius: 3mm, inset: 5mm, width: 100%,
    // No title for an empty one. The third item marks a caveat block (body
    // size title). Code mode, since markup adds stray space.
    stack(spacing: 5mm, ..blocks.map(b => {
      if b.at(0) != "" {
        text(weight: "bold", size: if b.at(2) { 1em } else { fw-h3 }, b.at(0))
      }
      if b.at(1).len() > 0 {
        v(1mm)
        b.at(1).join(parbreak())
      }
    })),
  )
}
