module Day11


// --- Day 11: Monkey in the Middle ---

exception UnableToParseMonkeyError of string

let LogLevel = 0

let Log lvl msg =
    if lvl = LogLevel then printfn msg else ()

type Monkey =
    { Id: int
      mutable Items: uint64[]
      Operation: uint64 -> uint64
      Test: int
      ThrowsTo: int * int }

    static member Start =
        { Id = 0
          Items = [||]
          Operation = (fun i -> i + 1UL)
          Test = 1
          ThrowsTo = (-1, -1) }

    member this.inspect (commonDevisor: int) (reduceWorryLevelFactor: int) (item: uint64) : uint64 =
        let newWorryLevel = this.Operation item
        Log 1 $"    Worry level is set to {newWorryLevel}."
        let reducedWorryLevel = newWorryLevel / (uint64 reduceWorryLevelFactor)

        Log
            1
            $"    Monkey gets bored with item. Worry level is divided by {reduceWorryLevelFactor} to {reducedWorryLevel}."

        reducedWorryLevel % (uint64 commonDevisor)

    member this.throw(item: uint64) : int =
        let f, t = this.ThrowsTo
        let testResult = (item % (uint64 this.Test)) = 0UL

        if testResult then
            Log 1 $"    Current worry level is divisible by {this.Test}"
            t
        else
            Log 1 $"    Current worry level is not divisible by {this.Test}"
            f


let (|Monkey|_|) (input: string) =
    if input.StartsWith "Monkey " then
        let id = input[7..]
        Some (int id[.. id.Length - 2])
    else
        None

let (|StartingItems|_|) (input: string) =
    if input.StartsWith "  Starting items: " then
        let items = input[ 18.. ].Split ',' |> Array.map int |> Array.map uint64
        Some items
    else
        None

let (|Operation|_|) (input: string) =
    if input.StartsWith "  Operation: new = old + " then
        let num = uint64 (int input[25..])
        Some (fun (i: uint64) -> num + i)
    elif input.StartsWith "  Operation: new = old * old" then
        Some (fun (i: uint64) -> i * i)
    elif input.StartsWith "  Operation: new = old * " then
        let num = uint64 (int input[25..])
        Some (fun (i: uint64) -> num * i)
    else
        None

let (|Test|_|) (input: string) =
    if input.StartsWith "  Test: divisible by " then
        Some (int input[21..])
    else
        None

let (|ThrowTo|_|) (input: string) =
    if input.StartsWith "    If true: throw to monkey " then
        Some (int input[29..])
    elif input.StartsWith "    If false: throw to monkey " then
        Some (int input[30..])
    else
        None

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n\n"

let parseMonkey (monkey: Monkey) (input: string) : Monkey =
    match input with
    | Monkey i -> { monkey with Id = i }
    | StartingItems items -> { monkey with Items = items }
    | Operation f -> { monkey with Operation = f }
    | Test f -> { monkey with Test = f }
    | ThrowTo i ->
        let a, b = monkey.ThrowsTo

        if b < 0 then
            { monkey with ThrowsTo = (a, i) }
        else
            { monkey with ThrowsTo = (i, b) }
    | _ -> raise (UnableToParseMonkeyError $"Cannot parse input: {input} for monkey: {monkey}")

let createMonkey (input: string) : Monkey =
    input |> (fun s -> s.Split "\n") |> Seq.fold parseMonkey Monkey.Start

let play (rounds: int) (reduceWorryLevelFactor: int) (monkeys: Monkey list) : uint list =
    let mutable inspectionCounter = Array.map (fun _ -> 0u) (Array.ofList monkeys)

    let logRounds =
        [ 1; 20; 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]

    let commonDevisor = monkeys |> List.fold (fun s m -> s * m.Test) 1

    for r = 1 to rounds do
        for m = 0 to monkeys.Length - 1 do
            let monkey = monkeys[m]
            Log 1 $"Monkey {m}:"

            let itemCount = monkey.Items.Length

            for item in monkey.Items do
                Log 1 $"  Monkey inspects an item with a worry level of {item}."
                let newItem = monkey.inspect commonDevisor reduceWorryLevelFactor item
                let newOwnerId = monkey.throw newItem
                Log 1 $"    Item with worry level {newItem} is thrown to monkey {newOwnerId}."
                monkeys[newOwnerId].Items <- Array.append monkeys[newOwnerId].Items [| newItem |]

            inspectionCounter[m] <- inspectionCounter[m] + (uint itemCount)
            monkey.Items <- Array.empty

        Log 2 $"After round {r}, the monkeys are holding items with these worry levels: "

        for monkey in monkeys do
            let itemsString = monkey.Items |> Array.fold (fun s a -> $"{s}{a}, ") ""
            Log 2 $"Monkey {monkey.Id}: {itemsString[.. itemsString.Length - 3]}"

        Log 2 "\n"

        if List.contains r logRounds then
            Log 3 $"== After round {r} =="

            for m = 0 to monkeys.Length - 1 do
                Log 3 $"Monkey {m} inspected items {inspectionCounter[m]} times."

            Log 3 "\n"

    inspectionCounter |> List.ofArray

let solvePart1 (input: seq<string>) : uint =
    input
    |> Seq.map createMonkey
    |> List.ofSeq
    |> play 20 3
    |> List.sortDescending
    |> List.take 2
    |> List.fold (*) 1u

let solvePart2 (input: seq<string>) : uint64 =
    input
    |> Seq.map createMonkey
    |> List.ofSeq
    |> play 10000 1
    |> List.sortDescending
    |> List.take 2
    |> List.fold (fun s a -> s * (uint64 a)) 1UL
