module Day04

open System.Text.RegularExpressions

// --- Day 4: Camp Cleanup ---

let processPair (line: string) : Set<int> * Set<int> =
    let r = Regex.Match (line, "(\d+)-(\d+),(\d+)-(\d+)")

    (seq { int r.Groups[1].Value .. int r.Groups[2].Value } |> Set.ofSeq,
     seq { int r.Groups[3].Value .. int r.Groups[4].Value } |> Set.ofSeq)

let preprocess (puzzle: string) : (Set<int> * Set<int>)[] =
    puzzle.TrimEnd().Split ("\n") |> Array.map processPair

let solvePart1 (input: (Set<int> * Set<int>)[]) : int =
    input
    |> Array.filter (fun (a, b) -> a.IsSubsetOf (b) || a.IsSupersetOf (b))
    |> Array.length

let solvePart2 (input: (Set<int> * Set<int>)[]) : int =
    input |> Array.filter (fun (a, b) -> not (a = a - b)) |> Array.length
