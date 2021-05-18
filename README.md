# parsegen

parsegen is an LR parser generator, similar to happy, ocamlyacc, and lalrpop.

It currently generates canonical LR(1) parsers, but LALR(1) and GLR are
planned.

Currently under development. Expect bugs and slow compile times. Also, it has
no documentation. All of these will be fixed.

See my [MinCaml parser][1] as an example parser implemented using parsegen.

[1]: https://github.com/osa1/mincaml/blob/master/src/parser.rs
