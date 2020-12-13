module Day6

    open System

    let blocks = Fixture.blocks 6

    let countDistinctAnswers (items : string array) : int =
        Seq.concat items
        |> Seq.distinct
        |> Seq.length

    let solve () = 
        blocks
        |> Array.map countDistinctAnswers
        |> Array.sum

    let countCommonAnswers (items : string array) : int =
        items
        |> Array.map Set.ofSeq
        |> Set.intersectMany
        |> Set.count

    let solve2 () = 
        blocks
        |> Array.map countCommonAnswers
        |> Array.sum