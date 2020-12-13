module Util

    let split2 (character : char) (text : string) : (string * string) option =
        match text.Split(character) with
        | [| x; y |] -> Some(x, y)
        | _ -> None

    let strSplit2 (separator : string) (text : string) : (string * string) option =
        match text.Split(separator) with
        | [| x; y |] -> Some(x, y)
        | _ -> None

    let countIf<'t> (pred : 't -> bool) (items : 't seq) : int =
        Seq.filter pred items |> Seq.length

    let tap<'t> (f : 't -> unit) (x : 't) =
        f x
        x

    let histogram<'t when 't : comparison> (xs : 't seq) : Map<'t, int> =
        let histo = new System.Collections.Generic.Dictionary<'t, int>()
        let update (item : 't) : unit =
            match histo.TryGetValue item with
            | (true, cnt) -> histo.[item] <- cnt + 1
            | _ -> histo.Add (item, 1)

        Seq.iter update xs
        Seq.map (fun (kvp : System.Collections.Generic.KeyValuePair<'t, int>) -> (kvp.Key, kvp.Value)) histo
        |> Map.ofSeq

    let private normalize<'t> : 't list -> 't array = 
        List.toArray >> tap System.Array.Reverse

    let splitBy<'t> (pred : 't -> bool) (items : 't seq) : 't  array array =
        let proc (groups, cur) (item : 't) =
            if pred item then
                (cur :: groups, [])
            else
                (groups, item :: cur)

        let (ps, last) = Seq.fold proc ([], []) items
        let ps = if List.isEmpty last then ps else last :: ps

        ps
        |> List.map normalize
        |> normalize

    let intParse (str : string) : int option =
        let (result, x) = 
            System.Int32.TryParse(str, 
                                  System.Globalization.NumberStyles.Integer, 
                                  System.Globalization.CultureInfo.InvariantCulture)
        if result then Some x else None