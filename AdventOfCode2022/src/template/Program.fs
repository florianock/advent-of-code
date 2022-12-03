module AdventOfCode2022.DayXX.Program

open System.IO
open DayXX

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt" |> preprocess
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let resultPart1 = solvePart1 input
    let part1Duration = timer.Elapsed
    printfn $"Part 1: %d{resultPart1} (executed in %s{part1Duration.ToString()})"
    let resultPart2 = solvePart2 input
    timer.Stop()
    let part2Duration = timer.Elapsed - part1Duration
    printfn $"Part 2: %d{resultPart2} (executed in %s{part2Duration.ToString()})"
    printfn $"Total execution time: %s{timer.Elapsed.ToString()}"
    0
