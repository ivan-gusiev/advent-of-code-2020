[<EntryPoint>]
let main argv =
    Day5.tests ()
    let s = Day5.solve2 ()
    printfn "%O" s
    0 // return an integer exit code
