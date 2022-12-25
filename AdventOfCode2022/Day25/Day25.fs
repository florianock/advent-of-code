module Day25

// --- Day 25: Full of Hot Air ---

open Snafu

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

let solvePart1 (snafuNumbers: seq<string>) =
    snafuNumbers |> Seq.sumBy toDecimal |> fromDecimal

let solvePart2 (input: seq<string>) = "tbd"
