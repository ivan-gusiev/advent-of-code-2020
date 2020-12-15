namespace global

module Day9 =

    let lines = Fixture.lines 9
    

    let invalid (db : int64 array) (x : int) =
        let target = db.[x]
        let window = seq {x - 25 .. x - 1 }
        let pairs = seq { 
                        for x in window do 
                            for y in window do 
                                if db.[x] <> db.[y] then yield (db.[x], db.[y]) }
        Seq.exists (fun (x, y) -> x + y = target) pairs |> not

    let getNums () = Array.map (fun (x : string) -> System.Convert.ToInt64 x) lines

    let solve () = 
        let nums = getNums ()

        seq { 25..nums.Length-1 }
        |> Seq.find (invalid nums)
        |> fun x -> nums.[x]

    let solve2 () =
        let nums = getNums ()
        let invalid = solve ()
        let lastNum = nums.Length - 1

        let ranges = seq {
            for x in seq { 0 .. lastNum } do
                for y in seq { x+1 .. lastNum } do
                    yield seq { x..y }
        }

        let sumNums (range : int seq) : int64 = 
            range
            |> Seq.map (fun x -> nums.[x]) 
            |> Seq.sum

        let minMax (range : int seq) : int64 * int64 = 
            range
            |> Seq.map (fun x -> nums.[x]) 
            |> fun r -> (Seq.min r, Seq.max r)

        ranges 
        |> Seq.find (fun rng -> (sumNums rng) = invalid)
        |> minMax
        |> fun (a, b) -> a + b