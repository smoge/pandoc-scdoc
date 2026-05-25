---
title: Markdown Format Example
summary: Pandoc Markdown input to SCDoc conversion example.
categories:
  - Reference
related:
  - Classes/SinOsc
  - Classes/LFSaw
redirect: OldName
keywords:
  - markdown
  - pandoc
  - scdoc
---

# Description

This file demonstrates how Pandoc Markdown is converted to SCDoc. Headings map
to SCDoc structure, method sections become method tags, and links are resolved
to their anchors. Raw SCDoc blocks are passed through unchanged.

The goal isn't to reproduce SCDoc character-for-character. Instead, help files
are authored in Pandoc Markdown and the writer translates them into SCDoc.

Inline markup is preserved: *emphasis*, **strong**, `inline code`, and
~~strikethrough~~. Single 'quoted' and double "quoted" words are also
supported.

Inline math: $x^2 + y^2 = r^2$.

Display math:

$$\int_0^\infty f(x)\,dx$$

A raw SCDoc inline: `strong::pass-through::`{=schelp}.

An [internal link](#target-section).

A [link stripping HelpSource prefix](HelpSource/Classes/SinOsc).

A [link with extension stripped](Classes/SinOsc.html).

An [external link](https://supercollider.github.io).

An [external file link](file:///usr/share/doc/supercollider/index.html).

# Target Section

This heading is the landing pad for the internal link above. Since the
document links to `#target-section`, the writer emits an
`anchor::target-section::` after the section tag.

## Subsection

An H2 outside a method section becomes a `subsection::`.

### Subsubsection

An H3, and any deeper heading level, becomes a `subsubsection::`.

## Subsection with Anchor {anchor="my-subsec-anchor"}

An H2 with an `anchor=` attribute emits an explicit `anchor::` tag after
the subsection tag.

## my-anchor {.anchor}

A heading with the `.anchor` class emits only `anchor::my-anchor::` — no
subsection tag is produced.

# Class Methods

Inside this section, the hierarchy starts doing work. H2 headings become SCDoc
methods. H3 headings become arguments — unless the name's a method-local keyword
like `Returns` or `Discussion`, in which case they don't, they hold their own
ground.

## ar, kr

Multiple names, comma-separated, stay on the same `method::` line.

### freq

Frequency in Hz.

### width

Pulse width, from 0 to 1.

### Returns {text="a UGen"}

Returns the constructed UGen.

## setter_

A setter method; the trailing underscore is stripped to produce
`method:: setter`.

## Discussion

H2 named `Discussion` inside a method section emits `discussion::`, not
`method:: Discussion`.

Discussion of the class methods.

## privateHelper {.private}

Internal helper; not part of the public API.

## SinOsc.ar {.copymethod}

Copies documentation from `SinOsc`'s `ar` method.

## MyTree {.classtree}

Shows the class tree rooted at `MyTree`.

# Instance Methods

## play {.method}

Plays the synth.

### server

The server to play on.

# Examples

A teletype code block:

``` {.teletype}
sclang -e 'Server.default.boot'
```

A math code block:

``` {.math}
E = mc^2
```

A raw SCDoc block, passed through as-is:

```schelp
note::
This block passes through unchanged.
::
```

A note, written as a Markdown block quote:

> Boot the server before you run the sound. No server, no signal.

A warning block opened by a heading named `warning`.

## warning

Naive oscillators can alias when pushed hard above the comfortable range.

A nested list. Inner items are promoted to sibling `##` entries because
SCDoc has no nested list syntax:

- outer 1

  - inner a
  - inner b

- outer 2

A definition list:

Term A
:   Definition A.

Term B
:   Definition B1.
:   Definition B2.

A line block:

| Line one
| Line two
| Line three

An image with a `HelpSource/` prefix stripped:

![A figure caption](HelpSource/images/figure.png)
