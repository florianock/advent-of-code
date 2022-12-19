module Day19

// --- Day 19: Not Enough Minerals ---

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n\n"

let getQualityLevel blueprint = blueprint |> String.length

let solvePart1 (blueprints: seq<string>) =
    // answer is blueprints |> Seq.map (qualitylevel  * blueprint ID) |> Seq.sum
    blueprints |> Seq.map getQualityLevel |> Seq.sum

let solvePart2 (input: seq<string>) = 0
