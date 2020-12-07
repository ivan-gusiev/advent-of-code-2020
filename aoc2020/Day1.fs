module Day1

open System

let ints = Fixture.lines 1 |> Array.map Int32.Parse

let solve =
    let (x, y) = seq { for x in ints do for y in ints do yield x, y }
                     |> Seq.find (fun (x, y) -> x + y = 2020)
    x * y

let solve2 =
    let (x, y, z) = seq { for x in ints do for y in ints do for z in ints do yield x, y, z }
                        |> Seq.find (fun (x, y, z) -> x + y + z = 2020)
    x * y * z