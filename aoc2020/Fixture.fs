module Fixture

open System
open System.IO

let private filename (day : int) : string = 
    sprintf "input/%s.txt" ((day |> Convert.ToString).PadLeft(2, '0'))

let str (day : int) : string =
    day |> filename |> File.ReadAllText

let lines (day : int) : string array =
    day |> filename |> File.ReadAllLines

let blocks (day : int) : string array array =
    lines day |> Util.splitBy String.IsNullOrWhiteSpace