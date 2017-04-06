let (=>) left right = print_string (if left = right then "." else "F")

let () = print_endline "\n * * * "

module TestParsing = struct
  module Parsing = Semver.Parsing
  module Source = Semver.Parsing.Source
  let source = Source.of_string

  module TestSource = struct

    let () = ()
      ; Source.to_string (Source.of_string "hai") => "hai"
  end

  open Semver.Parsing

  let () = ()
    ; any (source "x") => Some ('x', Source.End)
    ; any (source "") => None

  let () = ()
    ; parse (satisfy ((=) 'h')) (source "hai") => Some 'h'
    ; parse (satisfy ((=) 'X')) (source "hai") => None

  let () = ()
    ; parse (range '2' '4') (source "1") => None
    ; parse (range '2' '4') (source "2") => Some '2'
    ; parse (range '2' '4') (source "3") => Some '3'
    ; parse (range '2' '4') (source "4") => Some '4'
    ; parse (range '2' '4') (source "5") => None
end

let version = Semver.Version.create
let parse = Semver.Version.parse

let () = ()
  ; print_newline ()
  ; parse "0.0.0" => Some (version 0 0 0)
  ; parse "1.0.0" => Some (version 1 0 0)
  ; parse "x.0.0" => None
  ; parse "1.1.0" => Some (version 1 1 0)
  ; parse "1.x.0" => None
  ; parse "1.1.1" => Some (version 1 1 1)
  ; parse "1.1.x" => None
  ; parse "1.0.0-a" => Some (version 1 0 0 ~pre_release:["a"])
  ; parse "1.0.0+a" => Some (version 1 0 0 ~build_metadata:["a"])
  ; parse "1.0.0-a+a" => Some (version 1 0 0 ~pre_release:["a"]
                                          ~build_metadata:["a"])
  ; parse "1.0.0+a-a" => None
