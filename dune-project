(lang dune 3.18)

(name yarrow)

(version "0.0.1")

(generate_opam_files true)

(source 
 (github tjxing/yarrow))

(authors "Tang Jiaxing <zzhiliao@163.com>")

(maintainers "Tang Jiaxing <zzhiliao@163.com>")

(license LGPL-2.1-only)

(package
 (name yarrow)
 (synopsis "yarrow.ml")
 (description "Yet another machine learning framework in OCaml")
 (depends ocaml)
 (tags
  ("machine learning" "deep learning")))
