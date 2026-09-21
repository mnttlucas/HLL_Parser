# HLL Parser

## Description

The aim of this project is to process HLL files, according to the HLL-LDD-pr4.0rc1 specification.
In particular, it can be used to build a syntax tree from files, with the aim of extracting statistical information or checking that the grammar is respected.

## Installation

Dependencies - opam packages :
- OCaml 4.13.1
- dune 3.17
- menhir 2.0
- alcotest 1.8.0

## Usage

Once everything is installed ;

(Optional) You should build the project with :
```bash
dune build
```

(Optional) You should clean the project with :
```bash
dune clean
```

(Optional) You should re generate the project documentation with :
```bash
dune build @doc
```
and check the documentation at :
```bash
_build/default/_doc/_html/index.html
```

You should test the project with :
```bash
dune runtest
```

You should run it (and automatically builds if there is any modification) with :
```bash
dune exec -- Parser -hll <path-to-HLL_file> [-check] [-help] [-print] [-smt <path-to-SMT-output> -mode <bv/int/...> -stop <s> -lookahead <l>]
```
Supported options are :
- -hll Set HLL input file
- -print Print the AST to stdout
- -smt Set SMT output file
- -mode Set SMT logic
- -stop Number of timesteps for the generated SMT
- -lookahead Lookahead for the generated SMT
- -help  Display this list of options