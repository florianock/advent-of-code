module Day04

let expandSections (x: string) =
    let list = x.Split("-") |> Array.map System.Int32.Parse
    seq { list[0] .. list[1] } |> Set.ofSeq

let processPair (line: string) =
    let pairs = line.Split(",") |> Array.map expandSections
    (pairs[0], pairs[1])

let preprocess (puzzle: string) = puzzle.TrimEnd().Split("\n") |> Array.map processPair

let solvePart1 (input: (Set<int> * Set<int>)[]) =
    input
    |> Array.filter (fun (a, b) -> a.IsSubsetOf(b) || a.IsSupersetOf(b))
    |> Array.length

let solvePart2 (input: (Set<int> * Set<int>)[]) =
    input
    |> Array.filter (fun (a, b) -> not(a.Equals(a - b) && b.Equals(b - a)))
    |> Array.length
