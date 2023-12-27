# Advent of Code (2023)

**Language**: OCaml 5.1

**Packages**:
* *Num* - used once, it was useful for solving a system of linear equations using arbitrarily large integers
* *Str* - used occasionally for input parsing

**Run** (in REPL):
```bash
ocaml day04a.ml
```

**Compile and Run**:
```bash
ocamlfind ocamlopt -o prog -package str -linkpkg day04a.ml

./prog
```

**Caveat**
* *Batteries* did not work with OCaml 5.1, it could not find some files during compilation. First I have manually copied them, then some days later I realized that *Batteries* is probably not needed at all.
* The OCaml Jupyter kernel does not work (as of Dec. 2023) with OCaml 5.1.
