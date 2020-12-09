module Util

    let split2 (character : char) (text : string) : (string * string) option =
        match text.Split(character) with
        | [| x; y |] -> Some(x, y)
        | _ -> None

    let countIf<'t> (pred : 't -> bool) (items : 't seq) : int =
        Seq.filter pred items |> Seq.length