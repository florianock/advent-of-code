module Day03

// --- Day 3: Rucksack Reorganization ---

let preprocess (problem: string) = problem.TrimEnd().Split("\n")

let priorityOf item = ['a'..'z'] @ ['A'..'Z'] |> Seq.findIndex (fun el -> el = item) |> (+) 1

let getCommonItem = Seq.map Set >> Set.intersectMany >> Seq.head

let solvePart1 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.map (fun s -> [|s[.. s.Length / 2 - 1]; s[s.Length / 2 ..]|] |> getCommonItem)
    |> Seq.sumBy priorityOf

let solvePart2 (elvenRucksacks: seq<string>) =
    elvenRucksacks
    |> Seq.chunkBySize 3
    |> Seq.map getCommonItem
    |> Seq.sumBy priorityOf
