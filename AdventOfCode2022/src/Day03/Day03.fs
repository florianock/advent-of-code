module Day03

// --- Day 3: Rucksack Reorganization ---

let preprocess (problem: string) = problem.TrimEnd().Split("\n")

let rec intersect xs ys =
    match xs, ys with
    | x::xs', y::ys' ->
        if   x = y then x :: intersect xs' ys'
        elif x < y then intersect xs' ys
        else            intersect xs  ys'
    | _ -> []

let intersectStrings (lst: seq<string>) =
    let sorted = lst |> Seq.map(fun el -> el.ToCharArray() |> Array.sort |> List.ofArray)
    let initial = Seq.head sorted
    sorted |> Seq.fold intersect initial

let toPriority (c: char) : int =
    ['a'..'z'] @ ['A'..'Z']
    |> Seq.findIndex (fun el -> c = el)
    |> (+) 1
    
let solvePart1 (input: seq<string>) =
    input
    |> Seq.map(fun s ->
        seq { s[..s.Length/2-1]; s[s.Length/2..] }
        |> intersectStrings
        |> List.head
        |> toPriority
        )
    |> Seq.sum

let solvePart2 (input: seq<string>) =
    input
    |> Seq.splitInto (Seq.length input / 3)
    |> Seq.map (fun s ->
        s
        |> intersectStrings
        |> List.head
        |> toPriority
        )
    |> Seq.sum
