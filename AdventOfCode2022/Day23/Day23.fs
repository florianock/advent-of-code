module Day23

// --- Day 23: Unstable Diffusion ---

type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let preprocess (puzzle: string) : char[,] = puzzle.TrimEnd().Split "\n" |> array2D

let preferredDirections = [| N; S; W; E; N; S; W; E; N; S |]

let isElf field (x, y) = field[x, y] = '#'

let freeToMove field (x, y) direction =
    match direction with
    | N
    | NE
    | NW ->
        isElf field (x - 1, y - 1)
        || isElf field (x, y - 1)
        || isElf field (x + 1, y - 1)
    | NE
    | E
    | SE ->
        isElf field (x + 1, y - 1)
        || isElf field (x + 1, y)
        || isElf field (x + 1, y + 1)
    | SE
    | S
    | SW ->
        isElf field (x + 1, y + 1)
        || isElf field (x, y + 1)
        || isElf field (x - 1, y + 1)
    | SW
    | W
    | NW ->
        isElf field (x - 1, y + 1)
        || isElf field (x - 1, y)
        || isElf field (x - 1, y - 1)

let step (field: char[,]) i = field

let countEmptySpaces (field: char[,]) =
    let mutable empty = 0

    for c in field do
        if c = '.' then
            empty <- empty + 1

    empty

let solvePart1 (input: char[,]) =
    (input, [ 1..10 ]) ||> List.fold step |> countEmptySpaces

let solvePart2 (input: char[,]) = 0
