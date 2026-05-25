---
title: Comprehensive
summary: All SCDoc elements in one reference file
categories:
  - Tests
  - Tests>Reference
related:
  - Classes/SinOsc
  - Classes/LFSaw
redirect: OldName
keywords:
  - comprehensive
  - reference
---

# Description

A paragraph with *emphasis*, **strong**, `inline code`, and ~~strikethrough~~.

Single 'quoted' and double "quoted" words.

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

This section is the target of the internal link above.

## Subsection

An H2 becomes a subsection.

### Subsubsection

An H3 (and any deeper level) becomes a subsubsection.

## Subsection with Anchor {anchor="my-subsec-anchor"}

An H2 with an `anchor=` attribute emits an anchor tag after the subsection tag.

# Class Methods

## ar, kr

Method with multiple comma-separated names.

### freq

Frequency in Hz.

### width

Pulse width, from 0 to 1.

### Returns {text="a UGen"}

Returns the constructed UGen.

## setter_ {.method}

A setter method; the trailing underscore is stripped from the name.

## Discussion

Discussion of the class methods.

## privateHelper {.private}

Internal helper; not part of the public API.

## MyClass someMethod {.copymethod}

Copied from MyClass.

## MyTree {.classtree}

Shows the class tree rooted at MyTree.

## my-anchor {.anchor}

Anchor point for internal links.

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

A raw SCDoc block (passed through as-is):

```schelp
note:: This block passes through unchanged. ::
```

A note (block quote):

> This is a note.

A warning block opened by a heading named `warning`.

## warning

This is a warning.

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

An image with a HelpSource prefix stripped:

![A figure caption](HelpSource/images/figure.png)
