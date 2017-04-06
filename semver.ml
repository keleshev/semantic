let (>>) f g x = g (f x)

module Option = struct
  let value ~default = function
    | None -> default
    | Some x -> x
end

module String = struct
  include String

  let to_list string =
    let rec go i tail =
      if i < 0 then tail else go (i - 1) (string.[i] :: tail) in
    go (length string - 1) []
end

module Char = struct
  include Char

  let to_string = String.make 1
end

module Parsing = struct
  module Source = struct
    type t = End | Cons of char * t Lazy.t

    let rec of_stream stream =
      try Cons (Stream.next stream, lazy (of_stream stream))
      with Stream.Failure -> End

    let of_string = Stream.of_string >> of_stream

    let rec to_string = function (* O(n^2) *)
      | End -> ""
      | Cons (char, lazy rest) -> Char.to_string char ^ to_string rest
  end

  type 'result t = Source.t -> ('result * Source.t) option

  let (>>=) parse f = fun source ->
    match parse source with
    | Some (result, remaining) -> f result remaining
    | None -> None

  let (>>>) parser1 parser2 = parser1 >>= fun _ -> parser2

  let (<|>) parser1 parser2 = fun source ->
    match parser1 source with
    | Some _ as result -> result
    | None -> parser2 source

  let return result source = Some (result, source)

  let eof = function
    | Source.End -> Some ((), Source.End)
    | _ -> None

  let option parser = (parser >>= fun x -> return (Some x)) <|> return None

  let any = function
    | Source.Cons (char, lazy source) -> Some (char, source)
    | Source.End -> None

  let zero : 'a t = fun _ -> None

  let satisfy predicate =
    any >>= (fun char -> if predicate char then return char else zero)

  let parse parser source =
    match parser source with
    | Some (result, _) -> Some result
    | None -> None

  let range left right =
    satisfy (fun char -> left <= char && char <= right)

  let char char = satisfy ((=) char)

  let digit = range '0' '9'
end

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

    let number =
      let offset = Char.code '0' in
      digit >>= fun d ->
      return (Char.code d - offset)

    let pre_release =
      char '-' >>>
      char 'a' >>= fun _ ->
      return ["a"]

    let build_metadata =
      char '+' >>>
      char 'a' >>= fun _ ->
      return ["a"]

    let option parser =
      option parser >>= fun result -> return (Option.value ~default:[] result)

    let version =
      number                >>= fun major ->
      char '.'              >>>
      number                >>= fun minor ->
      char '.'              >>>
      number                >>= fun patch ->
      option pre_release    >>= fun pre_release ->
      option build_metadata >>= fun build_metadata ->
      eof                   >>>
      return (create ~pre_release ~build_metadata major minor patch)
  end

  let parse string =
    Parsing.parse Parser.version (Parsing.Source.of_string string)
end