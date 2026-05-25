helpsource := env_var_or_default("SCDOC_HELPSOURCE", "/home/smoge/build/supercollider/HelpSource")

default:
  just --list

check:
  git diff --check
  cabal check
  just cabal-test

cabal-build:
  cabal build

cabal-test:
  cabal test pandoc-scdoc-test

stack-build:
  stack build

stack-test:
  stack test

stack-corpus-test:
  SCDOC_HELPSOURCE="{{helpsource}}" stack test

cabal-corpus-test:
  SCDOC_HELPSOURCE="{{helpsource}}" cabal test pandoc-scdoc-test

audit:
  SCDOC_HELPSOURCE="{{helpsource}}" SCDOC_AUDIT=1 stack test

audit-samples n="10":
  SCDOC_HELPSOURCE="{{helpsource}}" SCDOC_AUDIT=1 SCDOC_AUDIT_SAMPLES={{n}} stack test

output-dir:
  mkdir -p output

run file="examples/example.md" format="markdown" out="output/markdown.schelp": output-dir
  cabal build exe:pandoc-scdoc
  "$(cabal list-bin exe:pandoc-scdoc)" -f {{format}} "{{file}}" > "{{out}}"

run-md:
  just run examples/example.md markdown output/markdown.schelp

run-org:
  just run examples/example.org org output/org.schelp

run-rst:
  just run examples/example.rst rst output/rst.schelp
