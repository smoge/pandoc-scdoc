:title: Comprehensive RST Test
:summary: Exercises all RST constructs supported by the SCDoc writer.
:categories: Reference>Tests
:related: Classes/SinOsc
:redirect: Classes/OldComprehensive
:keyword: rst
:keyword: test
:keyword: comprehensive

Description
===========

A paragraph with *emphasis*, **strong**, and ``inline code``.

Inline math: :math:`x^2 + y^2 = r^2`.

Display math:

.. math::

   \int_0^\infty f(x)\,dx

An `external link <https://supercollider.github.io>`__.

An `internal link <#target-section>`__ to the section below.

.. _target-section:

Target Section
==============

This is the target of the internal link above.

Subsection
----------

An H2 outside a method section becomes a subsection.

Subsubsection
~~~~~~~~~~~~~

An H3 becomes a subsubsection.

Class Methods
=============

ar, kr
------

A band-limited oscillator at audio or control rate.

freq
~~~~

Frequency in Hz.

mul
~~~

Amplitude multiplier, default ``1``.

Returns
~~~~~~~

A UGen.

Discussion
~~~~~~~~~~

Use ``ar`` for audio-rate and ``kr`` for control-rate signals.

offset
------

Apply a DC offset.

amount
~~~~~~

The offset amount.

Instance Methods
================

play
----

Play on the default server.

server
~~~~~~

The server to play on.

Returns
~~~~~~~

A Synth.

Examples
========

A code block:

.. code-block::

   { SinOsc.ar(440, 0, 0.1) }.play

A teletype block:

.. code-block:: teletype

   sclang -e 'Server.default.boot'

A note:

.. note::

   Remember to boot the server first.

A warning:

.. warning::

   This UGen is not band-limited.

A raw SCDoc block:

.. raw:: schelp

   note::
   This passes through unchanged.
   ::

A table:

======  =====
Name    Value
======  =====
freq    440
amp     0.5
======  =====

A definition list:

Term A
   Definition for A.

Term B
   Definition for B.

A nested list:

- outer 1

  - inner a
  - inner b

- outer 2

An image:

.. image:: images/waveform.png
   :alt: A waveform
