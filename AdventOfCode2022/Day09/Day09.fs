module Day09

let preprocess (puzzle: string) : seq<string> =
    puzzle.TrimEnd().Split ("\n")
    |> Array.map (fun s ->
        s.Split (' ')
        |> function
            | a -> (a[0], a[1]))

let solvePart1 (input: seq<string>) =
    input
    0

let solvePart2 (input: seq<string>) = 0
