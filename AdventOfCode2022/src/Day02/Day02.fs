module Day02

let preprocess (problem: string) = problem.TrimEnd().Split("\n") |> Seq.map (fun l -> l.Split(' '))

let toIndexTuples (shapes: seq<string[]>) =
    shapes
    |> Seq.map(fun s ->
        (
            Array.findIndex (fun elem -> elem = s[0]) [|"A"; "B"; "C"|],
            Array.findIndex (fun elem -> elem = s[1]) [|"X"; "Y"; "Z"|]
        ))

let solve input scoreFunction =
    input
    |> toIndexTuples
    |> Seq.map scoreFunction
    |> Seq.sum


let getResultScore shapeIndexes =
    match shapeIndexes with
    | o, p when p = (o + 1) % 3 -> 6
    | o, p when o = p -> 3
    | _, _ -> 0

let solvePart1 (input: seq<string[]>) =
    solve input (fun (other, player) -> 1 + player + getResultScore (other, player))


let getPlayerShapeIndex (other, result) =
    if result = 0 then (other + 2) % 3
    elif result = 2 then (other + 1) % 3
    else other

let solvePart2 (input: seq<string[]>) =
    solve input (fun (other, result) -> 1 + getPlayerShapeIndex (other, result) + result * 3)
