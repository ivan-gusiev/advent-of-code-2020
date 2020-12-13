namespace global

type Bag = { color : string }

module Bag = 
    let mk color = { Bag.color = color }

    let parse (description : string) : Bag option =
        description.Replace("bags", "").Replace("bag", "").Trim() |> mk |> Some

type NumBags = { bag : Bag; num : int }

module NumBags =
    open FSharp.Text.RegexProvider
    open Optional

    type private NumBagsRegex = Regex< @"^(?<Number>\d+)\s+(?<Color>[\w\s]+)\s+bags?$" >
    let private parseRegex = NumBagsRegex ()

    let mk bag num = { NumBags.bag = bag; num = num }

    let getBag (numBags : NumBags) = numBags.bag

    let parse (description : string) : NumBags option =
        optional {
            let! result = parseRegex.TryTypedMatch(description)
            let! num = Util.intParse result.Number.Value
            return mk (Bag.mk result.Color.Value) num
        }
        

type Rule = { bag : Bag; may_contain : NumBags array }

module Rule =
    open Optional

    let mk bag may_contain = { Rule.bag = bag; may_contain = may_contain }

    let parse (rule_text : string) : Rule option =
        match Util.strSplit2 "contain" rule_text with
        | Some (bagString, containString) ->
            let bagString = bagString.Trim()
            let containString = containString.Trim().TrimEnd('.')
            optional {
                let! bag = Bag.parse bagString
                
                let! contains = if containString = "no other bags" then 
                                    Some [||]
                                else
                                    containString.Split(", ")
                                    |> Array.map NumBags.parse
                                    |> Optional.sequence

                return mk bag contains
            }
        | None -> None

module Day7 =

    let lines = Fixture.lines 7
    
    let input = "shiny gold"

    let rules = lines
                |> Seq.map (Rule.parse >> Option.get)
                |> Seq.map (fun rule -> (rule.bag.color, rule))
                |> Map.ofSeq

    let getChildBags (rule : Rule) =
        Array.map NumBags.getBag rule.may_contain

    let fullContents (bag : Bag) : Bag seq = 
        let rec impl (cur : Bag) (all : Bag Set) : Bag Set =
            if Set.contains cur all then all else
                match Map.tryFind cur.color rules with
                | Some rule ->
                    let allowedBags = getChildBags rule
                    let childBags = Array.fold (fun state x -> Set.union state (impl x state)) all allowedBags
                    Set.union (Set.ofSeq allowedBags) childBags
                | None -> all

        impl bag Set.empty
        |> Set.toSeq

    let solve () =
        let isInput bag = (bag.color = input)

        Map.toSeq rules
        |> Seq.map (fun (k, v : Rule) -> v.bag)
        |> Seq.map fullContents
        |> Seq.filter (Seq.exists isInput)
        |> Seq.length

    let allBags (rule : Rule) : Bag seq =
        rule.may_contain
        |> Seq.collect (fun nb -> Seq.replicate nb.num nb.bag)

    let countContents (bag : Bag) : int =
        let rec impl (cur : Bag) : int =
            match Map.tryFind cur.color rules with
            | Some rule ->
                let allowedBags = allBags rule
                let childBags = Seq.fold (fun state x -> state + impl x) 0 allowedBags
                (Seq.length allowedBags) + childBags
            | None -> 
                printfn "Missed the rules cache"
                0

        impl bag

    let solve2 () =
        countContents rules.[input].bag