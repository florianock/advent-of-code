module Day18

// --- Day 18: Boiling Boulders ---

let preprocess (puzzle: string) : (int * int * int) list =
    puzzle.TrimEnd().Split("\n")
    |> Array.map (fun c ->
        c.Split(',')
        |> function
            | [| x; y; z |] -> (int x, int y, int z)
            | p -> failwith $"Unable to interpret coordinates {p}")
    |> Array.toList

let solvePart1 input = 0

let solvePart2 input = 0
