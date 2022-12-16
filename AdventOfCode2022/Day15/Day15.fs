module Day15

// --- Day 15: Beacon Exclusion Zone ---

open System.Text.RegularExpressions

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split("\n")
    |> Array.toList
    |> List.map (fun l ->
        let r = Regex.Matches (l, "(x|y)=(-?\d+)")
        (int r[0].Groups[2].Value, int r[1].Groups[2].Value), (int r[2].Groups[2].Value, int r[3].Groups[2].Value))
    

let dist ((x1, y1), (x2, y2)) : int =
    (abs (x1 - x2)) + (abs (y1 - y2))

let solvePart1 (targetRow: int) (sensorsAndBeacons: (int * int) * (int * int) list) : int =
    sensorsAndBeacons
    |> List.map (fun ((x1, y1), (x2, y2)) -> (abs (x1 - x2) + (abs (y1 - y2))))
    // |> List.average
    targetRow

let solvePart2 targetRow input = 0 
