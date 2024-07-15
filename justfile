build:
    #!/usr/bin/env bash
    cabal build

test:
    #!/usr/bin/env bash
    cabal test

accept:
    #!/usr/bin/env bash
    cabal test --test-options=--accept

format:
    #!/usr/bin/env bash
    ormolu --mode inplace $(git ls-files '*.hs')
