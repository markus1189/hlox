build: format lint
    #!/usr/bin/env bash
    cabal build --enable-tests

buildloop:
    #!/usr/bin/env bash
    find . -name '*.hs' -or -name '*.cabal' -or -name "justfile" \! -ipath '*/build/*' | entr -r -c just buildloop_step

buildloop_step:
    #!/usr/bin/env bash
    TTE_EFFECT="$(tte -h | awk '/Effect:/,/[[:space:]]*{/' | grep -o '{.*}' | tr -d '{}' | tr ',' '\n' | shuf | head -n 1)"
    cabal build --enable-tests

    if [[ $? == "0" ]]; then
      notify-send -u low "HLox" "Build Ok"

      cabal test
      if [[ $? == "0" ]]; then

        notify-send -u low "HLox" "Tests Green"
        echo Using tte effect "'${TTE_EFFECT}'"
        figlet -f doom 'SUCCESS' | tte --anchor-canvas c --anchor-text c "${TTE_EFFECT}"
      else
        notify-send -u critical "HLox" "Tests Failed"
      fi
    else
      notify-send -u critical "HLox" "Build Failed"
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

lint:
    #!/usr/bin/env bash
    hlint .

check-format:
    #!/usr/bin/env bash
    ormolu --mode check $(find . -name '*.hs')

repl: format
    #!/usr/bin/env bash
    cabal repl

run: format
    #!/usr/bin/env bash
    cabal run
