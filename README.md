# CAMLNES
## Description
Computer science degree fourth year project. An NES emulator using OCaml (imperative style) and SDL 2. Only supports mapper 0, no sound. Super Mario Bros. is running correctly.

Project realized by:
- Adrian HEOUAIRI

## Setup
    sudo apt install opam; opam init
    git clone https://gaufre.informatique.univ-paris-diderot.fr/heouairi/heouairi-plong-2022.git
    cd heouairi-plong-2022/CAMLNES
    opam switch create .
    eval $(opam env)
    dune build
    dune exec _build/default/bin/main.exe <path to your .nes game>

    # Optional: install the VSCode "OCaml Platform" extension and its dependencies: opam install ocaml-lsp-server ocamlformat ocamlformat-rpc (in the same switch)
    # (If starting a new project, run "dune init project CAMLNES" on the CAMLNES folder)
    # To keep VSCode consistent: dune build --watch --terminal-persistence=clear-on-rebuild
    # To run tests: dune test -f
    # To format the code with ocamlformat: dune fmt
    # To generate the documentation for CAMLNES/lib: dune build @doc (goes to CAMLNES/_build/default/_doc/_html/index.html)
    # To generate the documentation for CAMLNES/test: dune build @doc-private (goes to CAMLNES/_build/default/_doc/_html/*@*)
