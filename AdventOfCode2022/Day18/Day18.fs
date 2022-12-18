module Day18

// --- Day 18: Boiling Boulders ---

type Cube = int * int * int

let preprocess (puzzle: string) : (int * int * int) array =
    puzzle.TrimEnd().Split("\n")
    |> Array.map (fun c ->
        c.Split(',')
        |> function
            | [| x; y; z |] -> (int x, int y, int z)
            | p -> failwith $"Unable to interpret coordinates {p}")

let inline adjacent (x1, y1, z1) (x2, y2, z2) =
    (x1 = x2 && y1 = y2 && abs (z1 - z2) = 1)
    || (x1 = x2 && abs (y1 - y2) = 1 && z1 = z2)
    || (abs (x1 - x2) = 1 && y1 = y2 && z1 = z2)

let getNeighbours cube cubes =
    cubes
    |> Array.map (fun c -> (c, adjacent c cube))
    |> Array.filter snd
    |> Array.map fst
    |> List.ofArray

let solvePart1 (cubes: Cube array) =
    cubes
    |> Array.map (fun c -> (c, getNeighbours c cubes))
    |> Array.map (fun p -> 6 - (p |> snd |> List.length))
    |> Array.sum

let inline roomForAir (x1, y1, z1) (x2, y2, z2) =
    (x1 = x2 && y1 = y2 && abs (z1 - z2) = 2)
    || (x1 = x2 && abs (y1 - y2) = 2 && z1 = z2)
    || (abs (x1 - x2) = 2 && y1 = y2 && z1 = z2)

let getOtherCubesWithRoomForAir (x, y, z) (cubes: Cube array) =
    cubes
    |> Array.map (fun c -> (c, roomForAir c (x, y, z)))
    |> Array.filter (fun (c, b) -> b = true && not(cubes |> Array.contains ))
    |> Array.map fst
    |> List.ofArray
    
    
    // air at (2, 2, 5)
    // 2,2,4
    // 2,2,6
    // 1,2,5
    // 3,2,5
    // 2,1,5
    // 2,3,5

let solvePart2 cubes =
    cubes
    |> Array.map (fun c -> (c, getOtherCubesWithRoomForAir c cubes))
    |> Array.map (fun p -> 6 - (p |> snd |> List.length))
    |> Array.sum