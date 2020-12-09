let tests =
    let cases = [ "byr:2002", true
                  "byr:2003", false
                  "hgt:"
                ]

[<EntryPoint>]
let main argv =
    let s = Day4.solve2
    printfn "%O" s
    0 // return an integer exit code
