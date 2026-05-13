:title: RST Format Example
:summary: Pandoc RST input to SCDoc conversion example.
:categories: Reference
:keywords: rst, pandoc, scdoc

Description
===========

This file walks Pandoc RST to SCDoc, and walks it clean. Headings become
structure. Method sections announce themselves as method tags. Links find their
anchors, every time, every time. And raw SCDoc — untouched, untouchable — passes
right through.

Run it with::

   pandoc2schelp -f rst example.rst > example.schelp


The goal isn't to mimic SCDoc character-for-character. The goal is to write help
files in Pandoc, clean, and let the writer lower the shape into SCDoc.

SCDoc metadata fields live at the top of the file, set with RST field-list
syntax, before any heading. The recognized fields: ``title``, ``summary``,
``categories``, ``related``, ``redirect``, ``keyword``, and ``keywords``.

The ``title::``, ``summary::``, ``categories::``, and ``keyword::`` values in
this file's SCDoc output come straight from the field-list lines up top.
``related::`` and ``redirect::`` work the same way — they're just not used here.

H1 headings named ``Description``, ``Class Methods``, ``Instance Methods``, or
``Examples`` become SCDoc structural tags. Other H1 headings? They become
``section::``.

Inline markup still speaks clearly: *emphasis*, **strong**, and ``inline code``.
RST has no standard strikethrough. Use a raw SCDoc passthrough if needed — the
Examples section shows how.

Inline math: :math:`x^2 + y^2 = r^2`

Display math:

.. math::

   \int_0^\infty f(x)\,dx

An `external link <https://supercollider.github.io>`__.

An `internal link <#target-section>`__ targeting a heading below.

.. _target-section:

Target Section
==============

This heading is the landing pad for the internal link above. The
``.. _target-section:`` directive is folded by Pandoc into the
identifier of this heading. Because this document also links to
``#target-section``, the writer emits an ``anchor::target-section::``
after the section tag.

Subsection
----------

An H2 outside a method section becomes a ``subsection::``.

Subsubsection
~~~~~~~~~~~~~

An H3, and any deeper heading level, becomes a ``subsubsection::``.

Class Methods
=============

Inside this section, the hierarchy starts doing work. H2 headings become
SCDoc methods. H3 headings become arguments, unless the heading name is a
method-local keyword such as ``Returns`` or ``Discussion``.

ar, kr
------

Multiple names, comma-separated, stay on the same ``method::`` line.

freq
~~~~

Frequency in Hz.

phase
~~~~~

Initial phase offset in radians.

Returns
~~~~~~~

A heading named ``Returns`` becomes ``returns::``.

A UGen.

Discussion
~~~~~~~~~~

A heading named ``Discussion`` becomes ``discussion::``.

Notes on the signal's phase behavior at audio rate.

offset
------

This H2 is inferred as ``method:: offset``.

amount
~~~~~~

The offset amount.

Instance Methods
================

play
----

This H2 is inferred as ``method:: play``.

server
~~~~~~

The server to play on.

group
~~~~~

The group to add the synth to.

Examples
========

A code block:

.. code-block::

   { SinOsc.ar(440, 0, 0.1) }.play

A teletype block, using the RST code-block language as a Pandoc class:

.. code-block:: teletype

   sclang -e 'Server.default.boot'

A note directive, mapped to ``note::``:

.. note::

   Boot the server before you run the sound. No server, no signal.

A warning directive, mapped to ``warning::``:

.. warning::

   Naive oscillators can alias when pushed hard above the comfortable range.

Pandoc's RST reader may include a generated admonition title such as
"Note" or "Warning". The writer discards that generated title and emits
only the directive body.

A raw SCDoc block passed through verbatim:

.. raw:: schelp

   note::
   This block passes through unchanged.
   ::

A nested list. Inner items are promoted to sibling ``##`` entries because
SCDoc has no nested list syntax:

- outer 1

  - inner a
  - inner b

- outer 2

A definition list:

Term A
   Definition for A.

Term B
   Definition for B.

An image via ``.. figure::``. The caption becomes the SCDoc image label:

.. figure:: images/figure.png

   A figure caption.

An inline image via ``.. image::``. The alt text becomes the label:

.. image:: images/logo.png
   :alt: Logo
