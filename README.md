# pandoc-schelp


A [pandoc](https://pandoc.org/) writer for SCDoc, SuperCollider's help
file format (`.schelp`).

Status: experimental. Tested only with markdown input. 

The mapping isn't one-to-one — modal tags can't nest, 
headings stand in for scdoc's method and argument tags, 
so these conventions are a first pass. 
Feedback welcome, especially on the attribute syntax.

## Build and run

```sh
stack build
stack run -- scdocfile.md > scdocfile.schelp
```

## Install and use as a command-line 

```sh
stack install
md2schelp input.md > output.schelp
md2schelp < input.md > output.schelp
```

## Mapping (still needs some standartization)

### Metadata (yaml fields)

YAML keys `title`, `summary`, `categories`, `related`, `redirect`, and
`keyword`/`keywords` become SCDoc metadata. 

For example:

```yaml
---
title: TestOsc
summary: A fictional oscillator
categories: UGens>Generators
keywords: [oscillator, test]
---
```

### Headings


Level-1 headings with SCDoc structural names (`# Description`,
`# Class Methods`, `# Instance Methods`, `# Examples`) map to the
corresponding tag. Other H1s become `section::`; H2 → `subsection::`;
H3+ → `subsubsection::`.

### Methods

SCDoc's per-method tags have no direct markdown equivalent, so we use
(at least for now) pandoc's attribute syntax:

```markdown
## ar {.method name="ar" args="freq, phase, mul, add"}
Audio-rate.

### freq {.argument}
Frequency in hzzz.
```

Recognized header classes: `.method`, `.argument`, `.returns`,
`.discussion`, `.private`, `.copymethod`, `.classtree`, `.anchor`,
`.no-anchor`.

