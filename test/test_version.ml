let () = print_endline __FILE__; at_exit print_newline
let (=>) left right = print_string (if left = right then "." else "F")

let version = Semantic.Version.create
let parse = Semantic.Version.parse

let () = ()
  ; parse "0.0.0" => Some (version 0 0 0)
  ; parse "1.0.0" => Some (version 1 0 0)
  ; parse "x.0.0" => None
  ; parse "1.1.0" => Some (version 1 1 0)
  ; parse "1.x.0" => None
  ; parse "1.1.1" => Some (version 1 1 1)
  ; parse "1.1.x" => None
  ; parse "1.0.0+a" => Some (version 1 0 0 ~build_metadata:["a"])
  ; parse "1.0.0-a+a" => Some (version 1 0 0 ~pre_release:["a"]
                                          ~build_metadata:["a"])

  ; parse "01.0.0" => None
  ; parse "-1.0.0" => None
  ; parse "123.0.0" => Some (version 123 0 0)

  ; parse "1.0.0-/" => None
  ; parse "1.0.0-0" => Some (version 1 0 0 ~pre_release:["0"])
  ; parse "1.0.0-9" => Some (version 1 0 0 ~pre_release:["9"])
  ; parse "1.0.0-:" => None
  ; parse "1.0.0-@" => None
  ; parse "1.0.0-A" => Some (version 1 0 0 ~pre_release:["A"])
  ; parse "1.0.0-Z" => Some (version 1 0 0 ~pre_release:["Z"])
  ; parse "1.0.0-[" => None
  ; parse "1.0.0-`" => None
  ; parse "1.0.0-a" => Some (version 1 0 0 ~pre_release:["a"])
  ; parse "1.0.0-z" => Some (version 1 0 0 ~pre_release:["z"])
  ; parse "1.0.0-{" => None
  ; parse "1.0.0--" => Some (version 1 0 0 ~pre_release:["-"])

  ; parse "1.0.0-" => None

  ; parse "1.0.0-0-9A-Za-z-" => Some (version 1 0 0 ~pre_release:["0-9A-Za-z-"])

  ; parse "1.0.0-a.b.c" => Some (version 1 0 0 ~pre_release:["a"; "b"; "c"])
  ; parse "1.0.0-foo.bar" => Some (version 1 0 0 ~pre_release:["foo"; "bar"])

  ; parse "1.0.0+a.b.c" => Some (version 1 0 0 ~build_metadata:["a"; "b"; "c"])

  ; parse "1.0.0+" => None
  ; parse "1.0.0+a-a" => Some (version 1 0 0 ~build_metadata:["a-a"])

  ; parse "1.0.0-0" => Some (version 1 0 0 ~pre_release:["0"])
  ; parse "1.0.0-00" => None
  ; parse "1.0.0--00" => Some (version 1 0 0 ~pre_release:["-00"])
  ; parse "1.0.0-00a" => Some (version 1 0 0 ~pre_release:["00a"])
  ; parse "1.0.0-01" => None
  ; parse "1.0.0-01a" => Some (version 1 0 0 ~pre_release:["01a"])
  ; parse "1.0.0--01" => Some (version 1 0 0 ~pre_release:["-01"])

  ; parse "1.0.0+-01" => Some (version 1 0 0 ~build_metadata:["-01"])

  ; parse "1.0.0@#$@#$" => None
  ; parse "@#$@#$" => None
