// typst-template.typ
// The FWISE PDF report's page frame and components, as a Quarto template
// partial. R/report_pdf.R copies this beside the generated report.qmd and
// Quarto splices it into the Typst document it hands to the Typst compiler.
//
// NO VALUES LIVE HERE. Every colour, size, logo path and piece of footer
// wording comes from fwise-tokens.typ, which fw_pdf_tokens() writes from
// R/brand.R, R/config.R and the copy deck at render time. So the report takes
// its colours from the same tokens as the app and cannot drift from them, and
// dev/check_literals.R has nothing here to police.
//
// THE BODY IS BUILT FROM THESE FUNCTIONS. R does not write Typst markup with
// text spliced into it; it writes calls to the functions below with every
// piece of text passed as a string literal (fw_typ_str() escapes it). A
// species name with an asterisk or a site called "#2" cannot turn into
// formatting, and escaping has one rule rather than Typst's dozen.

#import "fwise-tokens.typ": *

// ---- The page ----------------------------------------------------------------

// Every page: the tinted ground (no white - see R/brand.R), and a footer with
// the logos the site's own footer carries - the FWISE mark first (client,
// 21 Sept 2026: on every page, not only the letterhead), then Weird Fishes
// Advisory and the four collaborators - beside the page count.
#let fw-footer = context {
  line(length: 100%, stroke: 0.5pt + fw-border)
  v(2mm)
  grid(
    columns: (1fr, auto),
    align: (left + horizon, right + horizon),
    column-gutter: 6mm,
    // Fractional spacing: the logos spread to fill their column and close up
    // rather than run into the page count.
    stack(
      dir: ltr,
      spacing: 1fr,
      ..(fw-logo-mark, ..fw-logos).map(p => image(p, height: fw-logo-h)),
    ),
    text(size: fw-credit, fill: fw-ink-muted)[
      #fw-page-label #counter(page).display() #fw-of-label #counter(page).final().first()
    ],
  )
}

#let fwise-report(doc) = {
  set document(title: fw-doc-title)
  set page(
    paper: "a4",
    fill: fw-page-fill,
    margin: (x: fw-margin, top: fw-margin, bottom: fw-margin + 16mm),
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

#let fw-letterhead(title: "", subtitle: "", tagline: "") = {
  image(fw-logo-mark, height: 20mm)
  v(2mm)
  text(fill: fw-ink-muted)[#tagline]
  v(5mm)
  heading(level: 1)[#title]
  text(fill: fw-ink-muted)[#subtitle]
  v(3mm)
  line(length: 100%, stroke: 2pt + fw-teal)
}

// ---- Blocks ------------------------------------------------------------------

// A titled block with its qualification directly beneath the heading - above
// the figure, not below it, the same rule the page follows: a caveat printed
// under a chart is read after the reader has drawn their conclusion from it.
//
// STICKY, both of them. The heading and its note are carried onto the next
// page with whatever follows them, so a page never ends on a title whose
// chart is overleaf.
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

// The page's teal-tint strip: the five headline numbers.
// On paper the five sit in a row with the label under each number: beside
// it, as on screen, the longer labels wrapped mid-phrase at this width.
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

// All four outcomes, always, at zero if need be. The label and the count are
// text; the bar and its colour are decoration.
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

// One row of photographs, the page's tiles: picture, name, count, the outcome
// split as one bar, and the photograph's credit and licence, which travel with
// every image as a condition of using it.
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

// The app's table style: a sunken header row that repeats on every page the
// table runs onto, hairlines between rows, and figures right-aligned in the
// mono face so they compare down a column.
#let fw-table(headers, rows, num: (), widths: auto) = {
  let n = headers.len()
  let cols = if widths == auto { (auto,) * (n - 1) + (1fr,) } else { widths }
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
    stack(spacing: 5mm, ..blocks.map(b => [
      #text(weight: "bold", size: fw-h3)[#b.at(0)]
      #v(1mm)
      #b.at(1)
    ])),
  )
}
