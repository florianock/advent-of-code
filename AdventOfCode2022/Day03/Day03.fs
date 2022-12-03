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

let intersectRucksacks (rucksacks: seq<string>) =
    let sortedRucksacks = rucksacks |> Seq.map(fun el -> el.ToCharArray() |> Array.sort |> List.ofArray)
    let initialRucksack = Seq.head sortedRucksacks
    sortedRucksacks |> Seq.fold intersect initialRucksack

let priorityOf item = ['a'..'z'] @ ['A'..'Z'] |> Seq.findIndex (fun el -> el = item) |> (+) 1
    
let getPriorityOfCommonItem = intersectRucksacks >> List.head >> priorityOf

let solvePart1 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.map(fun rucksack ->
        seq { rucksack[..rucksack.Length/2-1]; rucksack[rucksack.Length/2..] } |> getPriorityOfCommonItem)
    |> Seq.sum

let solvePart2 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.splitInto (Seq.length elvenRucksacks / 3)
    |> Seq.map getPriorityOfCommonItem
    |> Seq.sum
