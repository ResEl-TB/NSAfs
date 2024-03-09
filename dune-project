(lang dune 3.9)
(using menhir 2.1)
(name nsafs)
(generate_opam_files true)

(license MIT)
(authors "Benjamin Somers <benjamin.somers@resel.fr>")
(maintainers "Benjamin Somers <benjamin.somers@resel.fr>")

(package
 (name nsafs)
 (synopsis "ResEl metrics collection agent")
 (description "This project is a FUSE filesystem allowing to collect, classify, compress, sort, and send metrics to a Warp 10 server")
 (depends dolog ezcurl ezgzip menhir ocamlfuse ppx_string toml))
