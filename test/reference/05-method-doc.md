---
title: LFPulse
summary: Pulse oscillator
categories:
  - UGens>Generators>Deterministic
  - UGens>Oscillators
related: Classes/LFSaw
---

# Description

A non-band-limited pulse oscillator. Outputs a high value of one and a low
value of zero.

# Class Methods

## ar, kr

Keyword argument frequency.

### freq

Frequency in Hz.

### iphase

Initial phase offset.

### Returns

A UGen.

# Examples

```supercollider
{ LFPulse.ar(400) }.play
```
