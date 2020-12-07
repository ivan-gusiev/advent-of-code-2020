module Day2

open System
open System.Text.RegularExpressions

type Password = string

type PasswordPolicy = { character : char; min : int; max : int }

let password_policy_regex : Regex = new Regex("^(\d+)-(\d+) (.)$", RegexOptions.Compiled)

let parse_password_policy (policy : string) : PasswordPolicy option =
    let match_result = password_policy_regex.Match policy
    if match_result.Success then 
        let match_min = match_result.Groups.[1].Value |> Int32.Parse
        let match_max = match_result.Groups.[2].Value |> Int32.Parse
        let match_char = match_result.Groups.[3].Value.[0]

        { character = match_char; min = match_min; max = match_max } |> Some
    else 
        None

type DebugStatement = PasswordPolicy * Password

let parse_any_statement<'a> (parse_policy : string -> 'a option) (statement : string) : ('a * string) option =
    let parts = statement.Split(':')
    if parts.Length = 2 then
        let policy = parse_policy parts.[0]
        let pw = parts.[1]
        Option.map (fun x -> (x, pw)) policy
    else
        None

let parse_debug_statement = parse_any_statement parse_password_policy

let check_debug_statement ((policy, password) : DebugStatement) : bool =
    let char_count = Seq.filter (fun x -> x = policy.character) password
                     |> Seq.length
    char_count <= policy.max && char_count >= policy.min

let debug_strings : string array = Fixture.lines 2

let solve =
    debug_strings
    |> Array.map (parse_debug_statement >> Option.get) // assume statements are valid
    |> Array.filter check_debug_statement
    |> Array.length

(* ------------------------------------------------------------- *)

type NewPasswordPolicy = { character : char; pos1 : int; pos2 : int }

let parse_new_password_policy (policy : string) : NewPasswordPolicy option =
    let match_result = password_policy_regex.Match policy
    if match_result.Success then 
        let match_p1 = match_result.Groups.[1].Value |> Int32.Parse
        let match_p2 = match_result.Groups.[2].Value |> Int32.Parse
        let match_char = match_result.Groups.[3].Value.[0]

        { character = match_char; pos1 = match_p1; pos2 = match_p2 } |> Some
    else 
        None

type NewDebugStatement = NewPasswordPolicy * Password

let parse_new_debug_statement = parse_any_statement parse_new_password_policy

let check_new_debug_statement ((policy, password) : NewDebugStatement) : bool =
    let c1 = password.[policy.pos1] // -1 because off-by-one, +1 because space is still there
    let c2 = password.[policy.pos2]
    (c1 = policy.character) <> (c2 = policy.character)

let solve2 =
    debug_strings
    |> Array.map (parse_new_debug_statement >> Option.get) // assume statements are valid
    |> Array.filter check_new_debug_statement
    |> Array.length