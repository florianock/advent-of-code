module Day03

let preprocess (problem: string) = problem.TrimEnd().Split("\n") |> Seq.map (fun l -> l.Split(' '))

let solvePart1 (input: seq<string[]>) = 0

let solvePart2 (input: seq<string[]>) = 0