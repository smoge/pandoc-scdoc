# pandoc-scdoc

A [Pandoc](https://pandoc.org/) writer for SCDoc, SuperCollider's help file
format (`.schelp`).

Write the help file in a Pandoc format. Let the writer lower it into SCDoc.

Status: experimental. Tested with Markdown, RST, and org-mode input. Different
doors, same room.

Other Pandoc-supported formats may work to the extent their AST maps to the
supported nodes. The mapping is not one-to-one. SCDoc has its own constraints:
modal tags cannot nest, method and argument structure is tag-based, and lists
don't go deep. Not a mirror, a bridge.

## Build and run

With Stack:
```sh
stack build
mkdir -p output
stack run -- examples/example.md > output/markdown.schelp
stack run -- -f org examples/example.org > output/org.schelp
stack run -- -f rst examples/example.rst > output/rst.schelp
```

With Cabal:
```sh
cabal build
mkdir -p output
cabal run pandoc-scdoc -- examples/example.md > output/markdown.schelp
cabal run pandoc-scdoc -- -f org examples/example.org > output/org.schelp
cabal run pandoc-scdoc -- -f rst examples/example.rst > output/rst.schelp
```

## Install and use as a command-line tool

With Stack:
```sh
stack install
```

With Cabal:
```sh
cabal install pandoc-scdoc
```

Once installed:
```sh
pandoc-scdoc input.md > output.schelp
pandoc-scdoc -f rst input.rst > output.schelp
pandoc-scdoc -f org input.org > output.schelp
cat input.md | pandoc-scdoc > output.schelp
```

## Examples

Check the `examples/` folder:

- [`examples/example.md`](examples/example.md) for Markdown
- [`examples/example.rst`](examples/example.rst) for reStructuredText
- [`examples/example.org`](examples/example.org) for org-mode

Every format flows to the same beat: metadata, headings, methods, arguments,
links, anchors, code blocks, notes, warnings, lists, tables, images, and raw
SCDoc passthrough.

```sh
pandoc-scdoc examples/example.md > examples/example.schelp
pandoc-scdoc -f rst examples/example.rst > examples/example-rst.schelp
pandoc-scdoc -f org examples/example.org > examples/example-org.schelp
```

## Structural expectations

The writer expects documents to follow a SuperCollider-ish shape. Some headings
are semantic, not just visual.

- Method sections are functional. `# Class Methods` and `# Instance Methods`
  switch the writer into method inference: `## ar` becomes `method:: ar`, and
  `### freq` beneath it becomes `argument:: freq`.
- Heading levels are context-aware. Outside method sections, `##` is a
  `subsection::`. Inside a method section, the same `##` starts a method unless
  it is an explicit SCDoc keyword such as `Discussion` or `Returns`.
- Explicit classes win. Use attributes such as `{.method}`, `{.argument}`,
  `{.returns}`, or `{.discussion}` when you want to avoid inference.

## Mapping

### Metadata

SCDoc metadata:

- `title`
- `summary`
- `categories`
- `related`
- `redirect`
- `keyword` / `keywords`

The syntax depends on the input format.

#### Markdown metadata

Markdown uses YAML metadata:

```yaml
---
title: TestOsc
summary: A fictional oscillator
categories: UGens>Generators
related: Classes/SinOsc, Classes/Saw
keywords: [oscillator, test]
---
```

#### RST metadata

reStructuredText uses a leading field list. It must appear before headings.

```rst
:title: TestOsc
:summary: A fictional oscillator
:categories: UGens>Generators
:related: Classes/SinOsc, Classes/Saw
:keywords: oscillator, test
```

Recognized SCDoc fields lift into metadata. Unrecognized fields stay in the body
as a definition list.

#### Org-mode metadata

Org-mode uses `#+KEY: value` lines at the top of the file, before headings.
Repeat `#+KEYWORD:` for each keyword:

```org
#+TITLE: TestOsc
#+SUMMARY: A fictional oscillator
#+CATEGORIES: UGens>Generators
#+RELATED: Classes/SinOsc, Classes/Saw
#+KEYWORD: oscillator
#+KEYWORD: test
```

Unrecognized leading `#+` keys are discarded. Unlike RST, they do not stay.

### Headings

The shortcut: H1 headings with SCDoc structural names become SCDoc structural
tags.

```markdown
# Description
# Class Methods
# Instance Methods
# Examples
```

They become:

```schelp
description::
classmethods::
instancemethods::
examples::
```

Other H1 headings become `section::`.

Outside method sections, heading levels map like this:

```text
H1  -> section::
H2  -> subsection::
H3+ -> subsubsection::
```

Inside `Class Methods` and `Instance Methods`, the hierarchy does some work:

```text
H2 -> method::
H3 -> argument::
```

For example:

```markdown
# Class Methods

## ar, kr

### freq
Frequency in Hz.
```

Becomes:

```schelp
classmethods::

method:: ar, kr

argument:: freq

Frequency in Hz.
```

### Explicit SCDoc tags

When the input format can express Pandoc markdown attributes, special SCDoc tags
can be used directly:

- `.method`
- `.argument`
- `.returns`
- `.discussion`
- `.private`
- `.copymethod`
- `.classtree`
- `.anchor`
- `.no-anchor`

Example:

```markdown
## ar {.method}

### freq {.argument}

## SinOsc.ar {.copymethod}
```

Use explicit attributes as you wish. Use heading position when you want to read
clean.

For `copymethod::`, write the source as `Class.method` or `Class method` in the
heading text.

### Anchors and links

Markdown heading attributes can emit explicit SCDoc anchors:

```markdown
## Details {anchor="details"}
```

Emits:

```schelp
subsection:: Details

anchor::details::
```

The writer inserts anchors for headings referenced by local links when the
heading has a matching ID.

Pandoc folds the RST target into the heading identifier. Since the document
links to `#target-section`, the writer outputs:

```schelp
section:: Target Section

anchor::target-section::
```

Org-mode uses `:CUSTOM_ID:` for heading anchors. Same idea: the link calls the
name, the heading answers.

### Inline markup

Common inline markup maps to SCDoc modal tags:

```text
emphasis     -> emphasis::...::
strong       -> strong::...::
inline code  -> code::...::
inline math  -> math::...::
strikeout    -> soft::...::
links        -> link::...::
images       -> image::...::
```

SCDoc modal tags cannot nest, unlike Pandoc AST. When the source format nests
inline markup, the writer flattens the inside text.

### Code blocks

Ordinary code blocks become `code::` blocks:

````markdown
```supercollider
{ SinOsc.ar(440, 0, 0.1) }.play
```
````

Code blocks with class `teletype` become `teletype::` blocks:

````markdown
```teletype
scsynth -N score.osc _ out.aiff 48000 AIFF int24
```
````

Code blocks with class `math` become `math::` blocks:

````markdown
```math
E = mc^2
```
````

Code stays code.

### Tables

Tables become SCDoc `table:: ... ::` blocks.

Markdown example:

```markdown
| Name | Meaning         |
| ---- | --------------- |
| freq | Frequency in Hz |
| amp  | Amplitude       |
```

### Block quotes, notes, and warnings

Markdown block quotes become SCDoc notes:

```markdown
> Boot the server before you run the sound. No server, no signal, yo.
```

Headings named `warning` become `warning:: ... ::` blocks. The heading match
is case-insensitive, and the warning body is the next block after the heading:

```markdown
## warning

Naive oscillators can alias when pushed hard above the comfortable range.
```

Class `.warning` blocks are also supported when you need an explicit Pandoc
attribute form.

Other formats have their own way:

RST:

```rst
.. warning::

   Naive oscillators can alias when pushed hard above the comfortable range.
```

Org-mode:

```org
#+BEGIN_warning
Naive oscillators can alias when pushed hard above the comfortable range.
#+END_warning
```

### Definition lists

Definition lists become SCDoc `definitionlist:: ... ::` blocks.

RST example:

```rst
Term A
   Definition for A.

Term B
   Definition for B.
```

Org-mode example:

```org
- Term A :: Definition for A.
- Term B :: Definition for B.
```

### Images

Images become SCDoc `image::` tags. Captions or alt text become the SCDoc image
label when available.

Org-mode images must use the `file:` prefix so Pandoc recognizes them as images.
`#+CAPTION:` provides the SCDoc image label:

```org
#+CAPTION: A waveform
[[file:images/waveform.png]]
```

### Raw SCDoc passthrough

Raw SCDoc can pass through.

Markdown:

````markdown
```schelp
note::
This block passes through unchanged.
::
```
````

RST:

```rst
.. raw:: schelp

   note::
   This block passes through unchanged.
   ::
```

Org-mode:

```org
#+BEGIN_EXPORT schelp
note::
This block passes through unchanged.
::
#+END_EXPORT
```

No translation, no re-writing. Passed through as-is.

### Lists

Bullet and ordered lists become SCDoc `list::` and `numberedlist::` blocks.

Nested lists flatten to sibling entries — SCDoc has no nested-list syntax. If
the source goes down a level, the output brings it back up.

## Known limitations

The writer favors valid SCDoc over preserving every Pandoc detail. Keep these
constraints in mind when choosing an input format:

- Nested lists are flattened. If Markdown nests a list inside another list, the
  writer emits the nested items as sibling entries at the top level.
- Modal tags cannot nest. Inline combinations such as code inside strong
  emphasis are rendered with the outer style where possible, with the inner text
  flattened.
- Tables are simple grids. Multi-line cells, row spans, column spans, and rich
  block content inside table cells may be collapsed or omitted because SCDoc has
  no direct equivalent.
- Some unsupported nodes are silent. If an input feature has no semantic SCDoc
  equivalent, and is not raw SCDoc text, the writer may omit it so the generated
  `.schelp` file still compiles in the SuperCollider IDE.

## Format compatibility

The writer accepts any Pandoc-supported input format via `-f`:

```sh
pandoc-scdoc -f rst input.rst > output.schelp
pandoc-scdoc -f org input.org > output.schelp
```

Most mappings work from any format that Pandoc can parse into the right AST
nodes: headings, methods, arguments, tables, code blocks, links, block quotes,
images, and definition lists.

Some features are format-specific:

* Markdown uses YAML metadata and `{...}` heading attributes.
* RST uses leading field lists for metadata and `.. raw:: schelp` for raw
  passthrough.
* Org-mode uses `#+KEY: value` metadata, `:CUSTOM_ID:` heading properties, and
  `#+BEGIN_EXPORT schelp` for passthrough.
* HTML can express some structures directly, such as `<div class="warning">`.
