module Day04

let expandSections (sectionLimits: string): Set<int> =
    let limits = sectionLimits.Split("-") |> Array.map System.Int32.Parse
    seq { limits[0] .. limits[1] } |> Set.ofSeq

let processPair (line: string): Set<int> * Set<int> =
    let pair = line.Split(",") |> Array.map expandSections
    ( pair[0], pair[1] )

let preprocess (puzzle: string): (Set<int> * Set<int>)[]= puzzle.TrimEnd().Split("\n") |> Array.map processPair

let solvePart1 (input: (Set<int> * Set<int>)[]) =
    input
    |> Array.filter (fun (a, b) -> a.IsSubsetOf(b) || a.IsSupersetOf(b))
    |> Array.length

let solvePart2 (input: (Set<int> * Set<int>)[]) =
    input
    |> Array.filter (fun (a, b) -> not(a.Equals(a - b) && b.Equals(b - a)))
    |> Array.length
