namespace global

type Landscape = { data : string array }

type Territory = | Snow | Tree | EOF

module Landscape =

    let mk (data : string array) = { data = data }

    let width (landscape : Landscape) : int = landscape.data.[0].Length

    let at (x : int) (y : int) (landscape : Landscape) : Territory =
        let x = x % (width landscape)
        if y >= landscape.data.Length then EOF else
        match landscape.data.[y].[x] with
        | '.' -> Snow
        | _   -> Tree

module Day3 =

    let landscape_strings = Fixture.lines 3

    let landscape = Landscape.mk landscape_strings

    let slide (x, y) = (x + 3, y + 1)

    let solve =
        Seq.unfold (fun x -> (x, slide x) |> Some) (0, 0)
        |> Seq.map (fun (x, y) -> Landscape.at x y landscape)
        |> Seq.takeWhile (fun x -> x <> EOF)
        |> Seq.filter (fun x -> x = Tree)
        |> Seq.length

    (* ------------------------------------------------------- *)

    let solve_generic (slide_fn : int*int -> int*int) =
        Seq.unfold (fun x -> (x, slide_fn x) |> Some) (0, 0)
        |> Seq.map (fun (x, y) -> Landscape.at x y landscape)
        |> Seq.takeWhile (fun x -> x <> EOF)
        |> Seq.filter (fun x -> x = Tree)
        |> Seq.length

    let slide_generic (dx, dy) = fun (x, y) -> (x + dx, y + dy)

    let solve2 =
        [ 1, 1
          3, 1
          5, 1
          7, 1
          1, 2]
        |> Seq.map slide_generic
        |> Seq.map solve_generic
        |> Seq.reduce (*)
