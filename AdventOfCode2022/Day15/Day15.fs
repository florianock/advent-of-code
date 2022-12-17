module Day15

// --- Day 15: Beacon Exclusion Zone ---

open System.Text.RegularExpressions

type SensorData =
    { Position: int * int
      Beacon: int * int
      Distance: int
      Perimeter: (int * int) array }

    static member Default =
        { Position = (0, 0)
          Beacon = (0, 0)
          Distance = 0
          Perimeter = [||] }

    member this.covers(x, y) : bool =
        let a, b = this.Position

        x - this.Distance <= a
        && a <= x + this.Distance
        && y - this.Distance <= b
        && b <= y + this.Distance

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split("\n")
    |> Array.map (fun l ->
        let r = Regex.Matches(l, "(x|y)=(-?\d+)")

        { SensorData.Default with
            Position = (int r[0].Groups[2].Value, int r[1].Groups[2].Value)
            Beacon = (int r[2].Groups[2].Value, int r[3].Groups[2].Value) })

let inline dist (x1, y1) (x2, y2) : int = abs (x1 - x2) + abs (y1 - y2)

let coversRowFromDistance row sensor =
    let x, y = sensor.Position
    let minY = y - sensor.Distance
    let maxY = y + sensor.Distance

    if minY <= row && row <= maxY then
        let diffY = abs (y - row)
        let minXOnRow = x - sensor.Distance + diffY
        let maxXOnRow = x + sensor.Distance - diffY
        [| minXOnRow..maxXOnRow |]
    else
        [||]

let getRowCoverage targetRow (sensorData: SensorData array) =
    sensorData
    |> Array.Parallel.collect (coversRowFromDistance targetRow)
    |> Array.distinct

let getBeaconsOnRow targetRow sensorData =
    sensorData
    |> Array.map (fun s -> s.Beacon)
    |> Array.filter (fun (_, y) -> y = targetRow)
    |> Array.map fst
    |> Array.distinct

let solvePart1 (targetRow: int) sensorData : int =
    let sensorsWithDistances =
        sensorData
        |> Array.map (fun s -> { s with Distance = dist s.Position s.Beacon })

    let rowCoverage = getRowCoverage targetRow sensorsWithDistances
    let beaconsOnRow = getBeaconsOnRow targetRow sensorData
    (beaconsOnRow, rowCoverage) ||> Array.except |> Array.length

let solvePart2Slow (searchSpace: int) sensorsAndBeacons : uint64 =
    let mutable result = 0UL // 1246822400 is too low ; 16000000000000 is too high
    let mutable i = 0

    while i <= searchSpace && result = 0UL do
        let cov =
            getRowCoverage i sensorsAndBeacons
            |> Array.filter (fun p -> 0 <= p && p <= searchSpace)

        let foundGaps = (cov, [| 0..searchSpace |]) ||> Array.except

        foundGaps
        |> Array.iter (fun p -> result <- ((uint64 p) * 4_000_000UL) + (uint64 i))

        if i % 50 = 0 then printfn $"Finished row {i}" else ()
        i <- i + 1

    result

let fillCloudAndPerimeter searchSpace idx sensorData =
    printfn $"filling {idx}"
    let x, y = sensorData.Position
    let distance = sensorData.Distance

    let mutable perimeter =
        [| (x, y - distance - 1); (x, y + distance + 1) |]
        |> Array.filter (fun (_, y) -> 0 <= y && y <= searchSpace)
    // for i = (max 0 (y - distance)) to min searchSpace (y + distance) do
    //     let diffY = abs (y - i)
    //     let min = x - distance + diffY - 1
    //     let max = x + distance - diffY + 1
    //     perimeter <-
    //         [| (min, i); (max, i) |]
    //         |> Array.filter (fun (x, _) -> 0 <= x && x <= searchSpace)
    //         |> Array.append perimeter
    { sensorData with Perimeter = perimeter }

let tryFreePointOnPerimeter searchSpace sensors perimeter =
    let rec loop space lst points =
        match Array.tryHead points with
        | None -> None
        | Some  ->
            if 
            
    loop searchSpace sensors perimeter

let tryFindGap (searchSpace: int) (sensorData: SensorData[]) : (int * int) option =
    let rec loop space allSensors lst =
        match Array.tryHead lst with
        | None -> None
        | Some sensor ->
            let result = tryFreePointOnPerimeter space allSensors sensor.Perimeter
            match result with
            | Some p -> Some p
            | None -> loop space allSensors (Array.tail lst)
    
    loop searchSpace sensorData sensorData

let solvePart2 (searchSpace: int) (sensorData: SensorData array) : int =
    sensorData
    |> Array.map (fun s -> { s with Distance = dist s.Position s.Beacon })
    |> tryFindGap searchSpace
    |> function
        | Some(x, y) -> x * 4_000_000 + y
        | None -> failwith "No free point was found."


// |> Array.mapi (fillCloudAndPerimeter searchSpace)

// sensors
// |> Array.mapi (fun idx s ->
//     printfn $"folding {idx}"
//     (s.Perimeter, sensors)
//     ||> Array.fold (fun state c ->
//         state
//         |> Array.filter (fun p ->
//             not(c.covers p))))
// |> Array.concat
// |> Array.distinct
// |> Array.head
