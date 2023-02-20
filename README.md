# CAMLNES

## Setup
    sudo apt install opam; opam init
    git clone https://gaufre.informatique.univ-paris-diderot.fr/heouairi/heouairi-plong-2022.git
    cd heouairi-plong-2022/CAMLNES
    opam switch create .
    eval $(opam env)
    opam install dune # May not be required
    dune build
    dune exec _build/default/bin/main.exe <path to your .nes game>

    # Optional: install the VSCode "OCaml Platform" extension and its dependencies: opam install ocaml-lsp-server ocamlformat ocamlformat-rpc (in the same switch)
    # (If starting a new project, run "dune init project CAMLNES" on the CAMLNES folder)
    # To keep VSCode consistent: dune build --watch --terminal-persistence=clear-on-rebuild
    # To run tests: dune test -f
