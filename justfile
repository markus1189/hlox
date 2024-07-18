build: format
    #!/usr/bin/env bash
    cabal build --enable-tests

buildloop:
    #!/usr/bin/env bash
    find . -name '*.hs' -or -name '*.cabal' -or -name "justfile" \! -ipath '*/build/*' | entr -r -c just buildloop_step

buildloop_step:
    #!/usr/bin/env bash
    cabal build --enable-tests

    if [[ $? == "0" ]]; then
      notify-send -u low "HLox" "Build Ok"

      cabal test
      if [[ $? == "0" ]]; then

        notify-send -u low "HLox" "Tests Green"
        figlet -f doom 'SUCCESS' | tte --anchor-canvas c --anchor-text c beams
      else
        notify-send -u critical "HLox" "Tests Failed"
        figlet -f doom 'TEST FAIL' | tte --anchor-canvas c --anchor-text c decrypt

      fi
    else
      notify-send -u critical "HLox" "Build Failed"
      figlet -f doom 'BUILD FAIL' | tte --anchor-canvas c --anchor-text c decrypt
    fi

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
