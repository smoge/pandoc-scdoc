# Changelog - `pandoc-scdoc`

## Unreleased

## 0.2.0.0 - 2026-05-24

### Reader

Added `Text.Pandoc.Readers.SCDoc`, with `readSCDoc` and
`readSCDocPure`, for parsing `.schelp` files into a Pandoc AST.

The reader covers all 1137 files in SuperCollider's `HelpSource`;
1136/1137 currently round-trip cleanly through read -> write -> read.
The remaining case is `Overviews/JITLib.schelp`, a known
leading-whitespace quirk.

This release includes user-visible AST shape changes from 0.1.x:

- `returns::` and `note::` now use upstream-style body-bearing forms.
- `method::`, `private::`, and `copymethod::` get context classes
  from enclosing class/instance method sections.
- Multi-term definition rows preserve each term in the Pandoc AST.
- Body-level `keyword::` lines lift into `Meta`.
- Link canonicalization is shared between reader and writer.

### Writer

Improved round-trip behavior for definitions, code escaping, nested
lists, block-content table cells, warning headings, `copymethod::`,
math blocks, anchored links containing `#`, and `private::` setter
names.

In particular, headings titled `warning` with an explicit structural
class (`method`, `subsection`, ...) are no longer rewritten into
`warning::`; `.no-anchor` still permits the rewrite.

### Tooling

Added opt-in corpus audit coverage via `CorpusAuditSpec`. Enable with:

```sh
SCDOC_HELPSOURCE=/path/to/HelpSource SCDOC_AUDIT=1 stack test
```

The audit walks a local `HelpSource` checkout and writes an AST drift
report to `/tmp/scdoc-audit.md`.

## 0.1.0.1 - 2026-05-13

Patch release.

- Headings named `warning` are inferred as `warning::` blocks,
  wrapping the following block.
- Explicit `{.note}` / `{.warning}` divs and RST `.. note::` /
  `.. warning::` continue to map to SCDoc range tags.

## 0.1.0.0 - 2026-05-12

Initial release. Reads markdown, reStructuredText, org-mode, and other
Pandoc-supported formats; select via `-f [format]` (default markdown).

- Metadata fields `title`, `summary`, `categories`, `related`,
  `redirect`, and `keywords` emit as `key:: value` header lines, read
  from YAML frontmatter, RST docinfo, or org `#+KEY:`.
- Section kinds declared via Pandoc attributes (`## ar {.method}`)
  or inferred from structure: H2s under `ClassMethods` /
  `InstanceMethods` become `method::`; H3s inside a method become
  `argument::`. Section names match case- and whitespace-insensitively.
- `Description`, `ClassMethods`, `InstanceMethods`, `Examples` map
  to SCDoc keyword directives instead of generic `section::`.
- Tables render as `table::` blocks.
- Blockquotes become `note::` ranges; `{.note}` / `{.warning}` divs
  (including RST `.. note::` / `.. warning::`) map to the corresponding
  range tags.
- Fenced code blocks and inline raw spans tagged `schelp` or `scdoc`
  pass through verbatim.
- Headings targeted by local `#fragment` links get `anchor::` tags;
  `{.no-anchor}` suppresses this.
- Nested lists are flattened into `## ...` entries (SCDoc has no
  nested-list syntax).
- Smart typography is disabled so `--`, `...`, `'`, `"` stay as
  written.
