build: format
    #!/usr/bin/env bash
    cabal build

test: format
    #!/usr/bin/env bash
    cabal test

accept: format
    #!/usr/bin/env bash
    cabal test --test-options=--accept

format:
    #!/usr/bin/env bash
    ormolu --mode inplace $(git ls-files '*.hs')

check-format:
    #!/usr/bin/env bash
    ormolu --mode check $(find . -name '*.hs')

repl: format
    #!/usr/bin/env bash
    cabal repl

run: format
    #!/usr/bin/env bash
    cabal run
