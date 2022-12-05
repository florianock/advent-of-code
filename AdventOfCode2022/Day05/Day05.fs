module Day05

open Spectre.Console

// --- Day 5: Supply Stacks ---

let displayTitle input =
    AnsiConsole.Write(FigletText("--- Day 5: Supply Stacks ---").Centered())
    input

let readLines (puzzle: string) : seq<string>[] = puzzle.TrimEnd().Split("\n\n") |> Array.map (fun s -> s.Split("\n"))

let setupStack (stack: string[][]) (instruction: string): string[][] =
    [| for i = 0 to stack.Length - 1 do
        if instruction[4 * i + 1] = ' ' then yield stack[i]
        else yield Array.append [| instruction[4 * i + 1].ToString() |] stack[i] |]

let parse ([| stackSetup; instructions |]: seq<string>[]): seq<string> * string[][] =
    let numberOfStacks = ((stackSetup |> Seq.head |> String.length) + 1) / 4
    let emptyStack = Array.create numberOfStacks Array.empty
    let initialStack = stackSetup |> Seq.rev |> Seq.skip 1 |> Seq.rev |> Seq.fold setupStack emptyStack
    instructions, initialStack

let parseInstruction (instruction: string): int * int * int =
    let items = instruction.Split(' ')
    (int items[1], int items[3], int items[5])
    
let getTopCrates (stacks: string[][]): string = stacks |> Array.map Array.last |> Array.fold (fun a b -> a + b) ""

let applyInstruction (canHandleMultipleCratesAtOnce: bool) (stack: string[][]) ((c, f, t): int * int * int): string[][] =
    let fromStack = stack[f - 1]
    let cratesToMove = fromStack[(fromStack.Length - c) .. fromStack.Length]
    let newFromStack = fromStack[.. (fromStack.Length - c - 1)]
    let newToStack =
        cratesToMove
        |> (fun s -> if canHandleMultipleCratesAtOnce then s else Array.rev s)
        |> Array.append stack[t - 1]
    [| for i = 0 to stack.Length - 1 do
        if i = f - 1 then yield newFromStack
        elif i = t - 1 then yield newToStack
        else yield stack[i] |]

let followInstructions (canHandleMultipleCratesAtOnce: bool) (instructions: seq<string>, stack: string[][]): string[][] =
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold (applyInstruction canHandleMultipleCratesAtOnce) stack

let solvePart1: seq<string>[] -> string = displayTitle >> parse >> followInstructions false >> getTopCrates

let solvePart2: seq<string>[] -> string = displayTitle >> parse >> followInstructions true >> getTopCrates
