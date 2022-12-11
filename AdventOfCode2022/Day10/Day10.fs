module Day10

// --- Day 10: Cathode-Ray Tube ---

type CathodeRayDevice =
    { mutable Cycle: int
      mutable X: int
      mutable SavedXs: int list
      mutable Screen: string }

    static member Start =
        { Cycle = 0
          X = 1
          SavedXs = List.empty
          Screen = "" }

    static member ImportantCycles = [ 20; 60; 100; 140; 180; 220 ]

    member private this.saveXIfNecessary() =
        if List.contains this.Cycle CathodeRayDevice.ImportantCycles then
            printf $"Cycle {this.Cycle}; x = {this.X}. Adding {this.Cycle * this.X} to measurements {this.SavedXs}\n"

            this.SavedXs <- this.SavedXs @ [ this.Cycle * this.X ]

    member private this.updateScreen() =
        let screenPointer = this.Screen.Length % 40

        this.Screen <-
            if this.X - 2 < screenPointer && screenPointer < this.X + 2 then
                this.Screen + "#"
            else
                this.Screen + "."

    member this.tick() =
        this.Cycle <- this.Cycle + 1
        this.saveXIfNecessary ()
        this.updateScreen ()

    member this.addX(i) = this.X <- this.X + i
    member this.getChecksum() = this.SavedXs |> List.sum

    member this.getScreen() =
        List.ofSeq this.Screen
        |> List.chunkBySize 40
        |> List.map (fun s -> s |> List.toArray |> System.String)
        |> List.map (fun s -> s + "\n")
        |> List.fold (+) ""
        |> (fun s -> s[.. (s.Length - 2)])

let (|Addx|_|) (input: string) =
    if input.StartsWith "addx " then
        Some (int input[5..])
    else
        None

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

let runProgram (program: seq<string>) =
    let device = CathodeRayDevice.Start

    for cmd in program do
        match cmd with
        | "noop" -> device.tick ()
        | Addx i ->
            device.tick ()
            device.tick ()
            device.addX i
        | _ -> ()

    device

let solvePart1 (input: seq<string>) : int =
    let device = runProgram input
    device.getChecksum ()

let solvePart2 (input: seq<string>) : string =
    let device = runProgram input
    device.getScreen ()
