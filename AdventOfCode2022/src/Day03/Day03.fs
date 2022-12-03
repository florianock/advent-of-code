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
    let sortedListOfListOfChars = lst |> Seq.map(fun el -> el.ToCharArray() |> Array.sort |> List.ofArray)
    let initial = Seq.head sortedListOfListOfChars
    sortedListOfListOfChars |> Seq.fold intersect initial

let charPriority c = ['a'..'z'] @ ['A'..'Z'] |> Seq.findIndex (fun el -> c = el) |> (+) 1
    
let getGroupPriority = intersectStrings >> List.head >> charPriority

let solvePart1 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.map(
        fun rucksack -> seq { rucksack[..rucksack.Length/2-1]; rucksack[rucksack.Length/2..] } |> getGroupPriority)
    |> Seq.sum

let solvePart2 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.splitInto (Seq.length elvenRucksacks / 3)
    |> Seq.map getGroupPriority
    |> Seq.sum
