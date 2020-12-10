namespace global

type Ticket = { col : int; row : int; id : int }

module Ticket =
    let mk col row = { Ticket.col = col; row = row; id = col + 8 * row }

    let private codeToNum (up : string) (down : string) (code : string) : int =
        let rec impl (idx : int) (low : int) (hi : int) : int =
            let mid = (hi + low) / 2
            if idx >= code.Length then 
                low 
            else if code.[idx] = up.[0] then
                impl (idx + 1) (mid + 1) hi
            else
                impl (idx + 1) low mid
        
        let max = int (2. ** (float code.Length)) - 1
        impl 0 0 max

    let rec private generate (up : string) (down : string) (count : int) : string list =
        match count with
        | 0 -> []
        | 1 -> [up; down]
        | _ ->  [for x in [up; down] do for y in generate up down (count - 1) do yield x + y ]

    let lookup (up : string) (down : string) (num : int) : Map<string, int> =
        generate up down num
        |> List.map (fun s -> (s, codeToNum up down s))
        |> Map.ofList
    
    let rows = lookup "B" "F" 7
    let cols = lookup "R" "L" 3

    let parse (ticketCode : string) : Ticket option =
        if ticketCode.Length <> 10 then None else
            let row = rows.[ticketCode.Substring(0, 7)]
            let col = cols.[ticketCode.Substring(7, 3)]
            mk col row |> Some

module Day5 =

    let lines = Fixture.lines 5

    let tests () =
        Ticket.lookup "R" "L" 3
        |> Map.toList
        |> List.iter (printfn "%O")

        let case = "FBFBBFFRLR"
        printfn "%s -> %O" case (Ticket.parse case)

    let solve () =
        lines
        |> Array.map (Ticket.parse >> Option.get)
        |> Array.maxBy (fun x -> x.id)

    let solve2 () =
        let tickets = lines 
                      |> Array.map (Ticket.parse >> Option.get)
                      |> Array.map (fun t -> (t.id, t))
                      |> Map.ofArray

        let show c r =
            if Map.containsKey (r*8 + c) tickets then "x" else "."

        let printRow r =
            printf "%s" (r.ToString().PadLeft(3, '0'))
            List.iter (fun c -> (show c r |> printf "%s")) [0..7]
            printfn ""

        [0..127]
        |> List.iter printRow