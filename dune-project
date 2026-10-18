(lang dune 3.0)

(name contracts)

(generate_opam_files true)

(authors "Tave La Rocque")

(license MIT)

(package
 (name contracts)
 (synopsis
  "A Peyton Jones-Eber-Seward contract language, priced with option-implied densities")
 (depends
  (ocaml
   (>= 5.1))
  dune
  core
  ppx_jane))
