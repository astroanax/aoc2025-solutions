# Advent of Code 2025 solutions

My solutions for AOC2025, in rust and hardcaml (the janestreet challenge)
So far, I could only do 6 of them, (I began too late in jan 2026)

[This](https://ocamlstreet.gitbook.io/hardcaml-wiki/learn-hardcaml-in-half-an-hour#sequential-logic-memory) was a very helpful guide, and I found this [repo](https://github.com/MarcusCemes/advent-of-code-2025) where they tried solving in a hardware language too. I took some of them as inspiration.

If you're from jane street, use [this](https://deepwiki.com/astroanax/aoc2025-solutions/) to get nice cute diagrams of the state machines and all, bro

### Rust solutions

for running the rust solutions -
```
cd dayX/rust
rustc main.rs -o main && ./main < ../input1.txt
```

### Hardcaml

We use the [template project](https://github.com/janestreet/hardcaml_template_project/). There was some issue with `oxcaml_effect package`, which was fixed with  -

```
opam pin add oxcaml_effect.v0.18~preview.130.76+222 \
  https://github.com/janestreet/oxcaml_effect/archive/0216e836c8741c1fe9dd174f03c0cb384e8e0918.tar.gz -y
```

Run the solution with -
```
cd dayX/hardcaml
dune build
dune runtest
```
