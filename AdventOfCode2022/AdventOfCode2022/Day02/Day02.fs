module Day02

let preprocess (problem: string) = problem.TrimEnd().Split("\n") |> Seq.map (fun l -> l.Split(' '))

let toIndexTuples (shapes: seq<string[]>) =
    shapes
    |> Seq.map(fun s ->
        (
            Array.findIndex (fun elem -> elem = s[0]) [|"A"; "B"; "C"|],
            Array.findIndex (fun elem -> elem = s[1]) [|"X"; "Y"; "Z"|]
        ))

let getResultScore shapeIndexes =
    match shapeIndexes with
    | 0, 1 -> 6
    | 1, 2 -> 6
    | 2, 0 -> 6
    | 0, 0 -> 3
    | 1, 1 -> 3
    | 2, 2 -> 3
    | _, _ -> 0

let solvePart1 (input: seq<string[]>) =
    input
    |> toIndexTuples
    |> Seq.map (fun (other, player) -> 1 + player + getResultScore (other, player))
    |> Seq.sum

let getPlayerShapeIndex (other, result) =
    if result = 0 then (other + 2) % 3
    elif result = 2 then (other + 1) % 3
    else other

let solvePart2 (input: seq<string[]>) =
    input
    |> toIndexTuples
    |> Seq.map (fun (other, result) -> 1 + getPlayerShapeIndex (other, result) + result * 3)
    |> Seq.sum
