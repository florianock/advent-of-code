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

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

let solvePart1 (input: seq<string>) = 0

let solvePart2 (input: seq<string>) = 0

let test =
    @"....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."

let field = test.TrimEnd().Split "\n" |> Array.toList
let preferredDirections = [| N; S; E; W; N; S; E; W; N; S |] 

field
|> List.