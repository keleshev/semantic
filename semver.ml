open Shim

module Version = struct
  type t = {
    major: int;
    minor: int;
    patch: int;
    pre_release: string list;
    build_metadata: string list;
  }

  let create ?(pre_release=[]) ?(build_metadata=[]) major minor patch =
    {major; minor; patch; pre_release; build_metadata}

  module Parser = struct
    open Parsing

    let dot, plus, dash = char '.', char '+', char '-'

    module Identifier = struct
      let numeric_no_leading_zeroes = let open Parsing.Decimal in
        zero >=> return 0 <|> (
          cons non_zero (zero_or_more digit) >>= fun digits ->
          return (Int.of_string_exn (String.of_char_list digits)))

      let numeric_with_leading_zeroes = let open Parsing.Decimal in
        zero >=> Char.one_or_more digit

      let alpha_numeric_and_dash =
        letter <|> Decimal.digit <|> dash
    end

    (* let number = Identifier.numeric_no_leading_zeroes *)
    let number =
      let leading_zeroes = Identifier.numeric_with_leading_zeroes in
      not_followed_by (leading_zeroes >=> not_followed_by Decimal.digit) >=>
      Char.one_or_more Decimal.digit >>= fun digits ->
      return (Int.of_string_exn digits)

    let identifier =
      let valid_char = Identifier.alpha_numeric_and_dash in
      let leading_zeroes = Identifier.numeric_with_leading_zeroes in
      not_followed_by (leading_zeroes >=> not_followed_by valid_char) >=>
      Char.one_or_more valid_char

    let pre_release = dash >=> separated ~by:dot identifier

    let build_metadata = plus >=> separated ~by:dot identifier

    let option = default []

    let version =
      number                >>= fun major ->
      dot                   >=>
      number                >>= fun minor ->
      dot                   >=>
      number                >>= fun patch ->
      option pre_release    >>= fun pre_release ->
      option build_metadata >>= fun build_metadata ->
      eof                   >=>
      return (create ~pre_release ~build_metadata major minor patch)
  end

  let parse string =
    Parsing.parse Parser.version (Parsing.Source.of_string string)
end
