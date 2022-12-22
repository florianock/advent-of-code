module Day21

// --- Day 21: Monkey Math ---

// open Checked

type Operator =
    | Subtract
    | Add
    | Divide
    | Multiply

type MonkeyShout =
    | Primitive of int64
    | Operation of Operator * string * string
    | X

let (|Int|_|) str =
    match System.Int64.TryParse (str: string) with
    | true, i -> Some i
    | _ -> None

let (|Calculation|_|) (str: string) =
    let parts = str.Split ' '

    match parts with
    | [| monkeyA; op; monkeyB |] ->
        match op with
        | "-" -> (Subtract, monkeyA, monkeyB) |> Some
        | "+" -> (Add, monkeyA, monkeyB) |> Some
        | "/" -> (Divide, monkeyA, monkeyB) |> Some
        | "*" -> (Multiply, monkeyA, monkeyB) |> Some
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

let getOperator (op: Operator) =
    match op with
    | Add -> (+)
    | Subtract -> (-)
    | Divide -> (/)
    | Multiply -> (*)

let solve monkeys monkey =
    let rec go (lookup: Map<string, MonkeyShout>) (simian: string) =
        match lookup[simian] with
        | Primitive i -> i
        | Operation (op, simianA, simianB) -> (getOperator op) (go lookup simianA) (go lookup simianB)
        | X -> failwith "Can only solve for Primitive and Operation. You should use solveForX"

    go monkeys monkey

let solvePart1 (primates: Map<string, MonkeyShout>) = solve primates "root"

let trySolve monkeys monkey =
    try
        (solve monkeys monkey) |> Some
    with _ ->
        None

let inline getGoalForX (goal: int64) (operator: Operator, left: int64 option, right: int64 option) =
    if left.IsNone then
        let value = right.Value

        match operator with
        | Subtract -> goal + value
        | Add -> goal - value
        | Divide -> goal * value
        | Multiply -> goal / value
    else
        let value = left.Value

        match operator with
        | Subtract -> value - goal
        | Add -> goal - value
        | Divide -> value / goal
        | Multiply -> goal / value

let solveForX (goal: int64) (monkeys: Map<string, MonkeyShout>) (monkey: string) =
    let rec loop g (ms: Map<string, MonkeyShout>) m =
        let shout = ms[m]

        match shout with
        | X -> g
        | Operation (op, l, r) ->
            let maybeLeft = trySolve ms l
            let maybeRight = trySolve ms r
            let newGoal = getGoalForX g (op, maybeLeft, maybeRight)
            let direction = if maybeLeft.IsSome then r else l
            loop newGoal ms direction
        | Primitive i -> failwith $"Can't solve a Primitive for X! {i}; monkey {m}"

    loop goal monkeys monkey

let solvePart2 (wrongInput: Map<string, MonkeyShout>) =
    let left, right =
        match wrongInput["root"] with
        | Operation (_, l, r) -> l, r
        | _ -> failwith "this root is no good!"

    let input = wrongInput |> Map.remove "root" |> Map.add "humn" X

    let leftOption = trySolve input left
    let rightOption = trySolve input right

    if leftOption.IsSome then
        solveForX leftOption.Value input right
    else
        solveForX rightOption.Value input left
