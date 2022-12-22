module Day21

// --- Day 21: Monkey Math ---

open Checked

type MonkeyShout =
    | Primitive of uint64
    | Operation of (uint64 -> uint64 -> uint64) * string * string

let (|Int|_|) str =
    match System.Int32.TryParse (str: string) with
    | true, i -> Some (uint64 i)
    | _ -> None

let (|Calculation|_|) (str: string) =
    let parts = str.Split (' ')

    match parts with
    | [| monkeyA; op; monkeyB |] ->
        match op with
        | "-" -> Some ((-), monkeyA, monkeyB)
        | "+" -> Some ((+), monkeyA, monkeyB)
        | "/" -> Some ((/), monkeyA, monkeyB)
        | "*" -> Some ((*), monkeyA, monkeyB)
        | _ -> failwith $"Unknown operator {op}"
    | _ -> None

let parseShout shout =
    match shout with
    | Int i -> Primitive i
    | Calculation c -> Operation c
    | _ -> failwith $"This is bananas! {shout}"

let parseMonkey monkey =
    match monkey with
    | [| name; shout |] -> (name, parseShout shout)
    | _ -> failwith $"Unpardonable monkey input {monkey}"

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split "\n"
    |> Array.map (fun m -> m.Split (": ") |> parseMonkey)
    |> Map


let solvePart1 (primates: Map<string, MonkeyShout>) =
    let rec go (simian: string) (lookup: Map<string, MonkeyShout>) =
        let result = lookup[simian]

        match result with
        | Primitive i -> i
        | Operation (op, simianA, simianB) -> op (go simianA lookup) (go simianB lookup)

    go "root" primates

let solvePart2 (input: Map<string, MonkeyShout>) = 0
