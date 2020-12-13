namespace global

[<Struct>]
type OptionalBuilder =
  member __.Bind(opt, binder) =
    match opt with
    | Some value -> binder value
    | None -> None

  member __.Return(value) =
    Some value

  member __.Zero() = None

module Optional =
    let optional = new OptionalBuilder ()

    let sequence<'t> (xs : 't option array) : 't array option =
        if (Array.exists Option.isNone xs) then 
            None 
        else 
            Some (Array.map Option.get xs)