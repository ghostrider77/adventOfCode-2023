# Advent of Code (2023)

**Language**: OCaml 5.1

**Packages**:
* *Num* - used once, it was useful for solving a system of linear equations using arbitrarily large integers
* *Str* - used occasionally for input parsing

**Environment**

```bash
opam switch create aoc2023 ocaml-base-compiler.5.1.0
eval $(opam env --switch=aoc2023)
opam install -y utop merlin ocaml-lsp-server
```

**Run** (in REPL):
```bash
ocaml day01a.ml
```

**Compile and Run**:
```bash
# day04a.ml depends on Str
ocamlfind ocamlopt -o prog -package str -linkpkg day04a.ml

./prog
```

**Caveat**
* *Batteries* did not work with OCaml 5.1, it could not find some files during compilation. First I have manually copied them, then some days later I realized that *Batteries* is probably not needed at all.
* The OCaml Jupyter kernel does not work (as of Dec. 2023) with OCaml 5.1.
