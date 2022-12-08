module Day05

open System
open Spectre.Console

// --- Day 5: Supply Stacks ---

type Stack = string[]
type Stacks = Stack[]
type Move = (struct (int * int * int))

let displayTitle input =
    AnsiConsole.Write (FigletText("--- Day 5: Supply Stacks ---").Centered ())
    input

let readLines (puzzle: string) : seq<string>[] =
    puzzle.TrimEnd().Split (Environment.NewLine + Environment.NewLine)
    |> Array.map (fun s -> s.Split (Environment.NewLine) |> Seq.ofArray)

let setupStack (stacks: Stacks) (instruction: string) : Stacks =
    [| for i = 0 to stacks.Length - 1 do
           if instruction[4 * i + 1] = ' ' then
               yield stacks[i]
           else
               yield Array.append [| instruction[ 4 * i + 1 ].ToString () |] stacks[i] |]

let parse ([| stackSetup; instructions |]: seq<string>[]) : seq<string> * Stacks =
    let numberOfStacks = ((stackSetup |> Seq.head |> String.length) + 1) / 4
    let emptyStack = Array.create numberOfStacks Array.empty

    let initialStack =
        stackSetup |> Seq.rev |> Seq.skip 1 |> Seq.rev |> Seq.fold setupStack emptyStack

    instructions, initialStack

let parseInstruction (instruction: string) : Move =
    let items = instruction.Split (' ')
    struct (int items[1], int items[3], int items[5])

let getTopCrates (stacks: Stacks) : string =
    stacks |> Array.map Array.last |> Array.fold (fun a b -> a + b) ""

let applyInstruction (canHandleMultipleCratesAtOnce: bool) (stack: Stacks) ((c, f, t): Move) : Stacks =
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

let followInstructions (canHandleMultipleCratesAtOnce: bool) (instructions: seq<string>, stacks: Stacks) : Stacks =
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold (applyInstruction canHandleMultipleCratesAtOnce) stacks

let solvePart1: seq<string>[] -> string =
    displayTitle >> parse >> followInstructions false >> getTopCrates

let solvePart2: seq<string>[] -> string =
    displayTitle >> parse >> followInstructions true >> getTopCrates
