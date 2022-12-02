module AdventOfCode2022.Day02.Program

open System.IO
open Day02

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt" |> preprocess
    let resultPart1 = solvePart1 input
    printfn $"Part 1: %d{resultPart1}"
    let resultPart2 = solvePart2 input
    printfn $"Part 2: %d{resultPart2}"
    0
