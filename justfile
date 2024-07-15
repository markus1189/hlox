build:
    #!/usr/bin/env bash
    cabal build

test:
    #!/usr/bin/env bash
    cabal test

accept:
    #!/usr/bin/env bash
    cabal test --accept
