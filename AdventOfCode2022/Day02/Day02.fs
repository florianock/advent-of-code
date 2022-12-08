module Day02

// --- Day 2: Rock Paper Scissors ---

let preprocess (problem: string) = problem.TrimEnd().Split ("\n")

let toIndexTuples (shapes: seq<string>) =
    shapes |> Seq.map (fun s -> ("ABC".IndexOf (s[0]), "XYZ".IndexOf (s[2])))

let solve input scoreFunction =
    input |> toIndexTuples |> Seq.sumBy scoreFunction

let getResultScore shapeIndexes =
    match shapeIndexes with
    | p1, p2 when p2 = (p1 + 1) % 3 -> 6
    | p1, p2 when p1 = p2 -> 3
    | _, _ -> 0

let solvePart1 (input: seq<string>) =
    solve input (fun (p1, p2) -> 1 + p2 + getResultScore (p1, p2))

let getPlayerShapeIndex (p1, result) =
    if result = 0 then (p1 + 2) % 3
    elif result = 2 then (p1 + 1) % 3
    else p1

let solvePart2 (input: seq<string>) =
    solve input (fun (p1, result) -> 1 + getPlayerShapeIndex (p1, result) + result * 3)
