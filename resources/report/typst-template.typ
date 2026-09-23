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
// TWO ROWS, NOT ONE COLUMN EACH (client, 23 Sept 2026: bigger logos). The
// logos and the page count used to share a line, which capped the logo height
// at 8mm - any taller and they ran into the count. The count now sits under
// them on its own line, so the logos have the full text width and fw-logo-h
// is free to be the size the client asked for.
#let fw-footer = context {
  line(length: 100%, stroke: 0.5pt + fw-border)
  v(2mm)
  // Fractional spacing: the logos spread to fill the width and close up
  // rather than bunching at the left.
  stack(
    dir: ltr,
    spacing: 1fr,
    ..(fw-logo-mark, ..fw-logos).map(p => image(p, height: fw-logo-h)),
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
    // The bottom margin reserves the footer. It grew with the footer when the
    // logos went to 11mm on a line of their own (23 Sept 2026); too small a
    // value here and the last block of a page prints over the logos.
    margin: (x: fw-margin, top: fw-margin, bottom: fw-margin + 24mm),
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

// fw-mark-h, not the footer's fw-logo-h: the mark is the masthead of the
// document and the client asked for it bigger than the row of credits at the
// foot of every page (23 Sept 2026). Both come from fw_pdf_tokens().
//
// THE v(2.5mm) UNDER THE TITLE is the client's "little more space between the
// title and subtext". The level-1 show rule already puts 3mm below the
// heading block; this is on top of that, because at the title size 3mm read
// as the subtitle being part of the heading.
#let fw-letterhead(title: "", subtitle: "") = {
  image(fw-logo-mark, height: fw-mark-h)
  v(7mm)
  heading(level: 1)[#title]
  v(2.5mm)
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
// `size` sets the whole table's text. Only the report's "What this report
// covers" table passes one (fw-small); every other table inherits the
// document size, which is the print floor.
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
    stack(spacing: 5mm, ..blocks.map(b => [
      #text(weight: "bold", size: fw-h3)[#b.at(0)]
      #v(1mm)
      #b.at(1)
    ])),
  )
}
