namespace global

type FieldName = string
type FieldValue = string

type Field = { name : FieldName; value : FieldValue }

module Field =

    open System
    open System.Globalization
    open System.Text.RegularExpressions

    let mk (nm, value) : Field = { Field.name = nm; value = value }

    let parse (str : string) : Field option =
        Util.split2 ':' str
        |> Option.map mk

    let private bounded_int (value : FieldValue) (min : int) (max : int) : int option =
        match System.Int32.TryParse (value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | (true, x) when x >= min && x <= max -> Some x
        | _ -> None

    let private available = Option.isSome

    let private height (value : FieldValue) : bool =
        let last2 = value.Substring (value.Length - 2)
        let others = value.Replace(last2, "")
        match last2 with
        | "cm" -> bounded_int others 150 193 |> available
        | "in" -> bounded_int others  59  76 |> available
        | _ -> false

    let private in_set (value : FieldValue) (items : string seq) : bool =
        Seq.exists (fun x -> x = value) items

    let private hex_color (value : FieldValue) = Regex.IsMatch(value, "#[0-9a-f]{6,6}")

    let private passport_id (value : FieldValue) = Regex.IsMatch(value, "[0-9]{9,9}")

    let isValid (fld : Field) : bool =
        match fld.name with
        | "byr" -> bounded_int fld.value 1920 2002 |> available
        | "iyr" -> bounded_int fld.value 2010 2020 |> available
        | "eyr" -> bounded_int fld.value 2020 2030 |> available
        | "hgt" -> height fld.value
        | "hcl" -> hex_color fld.value
        | "ecl" -> in_set fld.value [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
        | "pid" -> passport_id fld.value
        | "cid" -> true
        | _ -> false

type Passport = Field array

module Passport =
    open System.Text.RegularExpressions

    let private field_regex : Regex = new Regex("\w+:[\w\#]+", RegexOptions.Compiled)

    let private parseline (line : string) : Field seq =
        let matches = field_regex.Matches line
        seq { 0..matches.Count - 1 }
        |> Seq.map (fun x -> matches.[x].Value)
        |> Seq.map (Field.parse >> Option.get)

    let parse (lines : string seq) : Passport =
        lines
        |> Seq.collect parseline
        |> Seq.toArray

    let containsKey (passport : Passport) (key : FieldName) : bool =
        Array.exists (fun f -> f.name = key) passport

    let hasAllFields (passport : Passport) : bool =
        let required_keys = [| "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" |]

        //printfn "Passport keys: %s" (System.String.Join(", ", Seq.map (fun x -> x.name) passport))

        let missing = Array.filter (containsKey passport >> not) required_keys
        //printfn "Missing keys: %s" (System.String.Join(", ", missing))

        let result = Array.forall (containsKey passport) required_keys
        //printfn "Result: %O" result
        //printfn ""

        result

    let isValid (passport : Passport) : bool =
        hasAllFields passport && Seq.forall Field.isValid passport

module Day4 =

    let raw_passport_strings = Fixture.lines 4

    type State = Passport list * string list

    let processLine ((passes, cur) : State) (line : string) : State =
        if System.String.IsNullOrWhiteSpace line then
            (Passport.parse (List.rev cur) :: passes, [])
        else
            (passes, line :: cur)

    let (ps, _) = Seq.fold processLine ([], []) raw_passport_strings

    let passports = Seq.rev ps

    let solve =
        printfn "Total passports: %i" (Seq.length passports)

        let valid = passports |> Seq.filter Passport.hasAllFields
        printfn "Valid passports: %i" (Seq.length valid)

        Util.countIf Passport.hasAllFields passports

    let solve2 = 
        Util.countIf Passport.isValid passports