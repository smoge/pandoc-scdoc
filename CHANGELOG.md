# Changelog `pandoc-scdoc`

## Unreleased

## 0.1.0.0 - 2026-05-12

- Markdown, reStructuredText, org-mode, and other Pandoc-supported formats
  are accepted as input. Select the source format with `-f [format]`. The
  default is markdown.

- Metadata keeps its shape on the way into SCDoc. Fields `title`,
  `summary`, `categories`, `related`, `redirect`, and `keywords` are emitted as
  `key:: value` header lines. They can be read from YAML frontmatter in
  markdown, a leading docinfo definition list in rst, or `#+KEY:` in org-mode.

- Sections can be declared directly with Pandoc attribute syntax, for example,
  `## ar {.method}` or `## freq {.argument}`. When no attribute is given, the
  writer infers from structure: H2s under `ClassMethods` or `InstanceMethods`
  become `method::` entries; H3s inside a method become `argument::` entries.
  Note: section names are matched case- and whitespace-insensitively, so `Class
  Methods` and `classmethods` have the same output.

- Recognized top-level sections — `Description`, `ClassMethods`,
  `InstanceMethods`, and `Examples` — map to their SCDoc keyword directives
  instead of generic `section::` entries.

- Tables are rendered as SCDoc `table::` blocks.

- Notes and warnings map directly to SCDoc range tags. Blockquotes become
  `note::` range tags; `{.note}` and `{.warning}` divs, including RST
  `.. note::` and `.. warning::` blocks, map to the corresponding SCDoc
  range tags.

- Raw SCDoc pass-through is supported for markup the converter should preserve
  rather than reinterpret. Fenced code blocks and raw inline spans tagged
  `schelp` or `scdoc` are emitted verbatim.

- Headings targeted by local `#fragment` links receive `anchor::` tags
  automatically. Only linked headings get anchors; use `{.no-anchor}` on a
  heading to suppress this.

- Nested bullet and ordered lists are flattened into `## …` entries, because
  SCDoc has no nested-list syntax.

- Smart typography (`Ext_smart`) is disabled so ASCII punctuation is preserved
  exactly as written: `--`, `...`, `'`, and `"` stay unchanged in the output.
