module Day24

let preprocess (puzzle: string) = puzzle.TrimEnd().Split "\n" |> array2D

let display (field: char[,]) : char[,] =
    let maxY = (field |> Array2D.length1) - 1

    [0 .. maxY]
    |> List.map (fun i ->
        let line =
            field[i, *]
            |> Array.map (fun c -> c.ToString())
            |> Array.fold (+) ""
        printfn $"{line}")
    |> ignore
    
    field

let inline nextHurricanePos (maxX, maxY) (x, y) direction =
    match direction with
    | '<' -> if x > 0 then (x - 1, y) else (maxX, y)
    | '>' -> if x < maxX then (x + 1, y) else (0, y)
    | '^' -> if y > 0 then (x, y - 1) else (x, maxY)
    | 'v' -> if y < maxY then (x, y + 1) else (x, 0)
    | _ -> failwith $"Invalid direction {direction}"

let solvePart1 (input: seq<string>) =
    0

let solvePart2 (input: seq<string>) = 0
