module Day15

// --- Day 15: Beacon Exclusion Zone ---

open System.Text.RegularExpressions

type SensorData =
    { Position: int * int
      Beacon: int * int
      Radius: int }

let preprocess (puzzle: string) =
    let defaultSensor =
        { Position = (0, 0)
          Beacon = (0, 0)
          Radius = 0 }

    puzzle.TrimEnd().Split("\n")
    |> Array.map (fun l ->
        let r = Regex.Matches(l, "(x|y)=(-?\d+)")

        { defaultSensor with
            Position = (int r[0].Groups[2].Value, int r[1].Groups[2].Value)
            Beacon = (int r[2].Groups[2].Value, int r[3].Groups[2].Value) })

let inline dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let coversRowFromDistance row sensor =
    let x, y = sensor.Position
    let minY = y - sensor.Radius
    let maxY = y + sensor.Radius

    if minY <= row && row <= maxY then
        let diffY = abs (y - row)
        let minXOnRow = x - sensor.Radius + diffY
        let maxXOnRow = x + sensor.Radius - diffY
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
        |> Array.map (fun s -> { s with Radius = dist s.Position s.Beacon })

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

let inline generatePerimeter searchSpace sensor : (int * int) seq =
    let x, y = sensor.Position
    let d = sensor.Radius

    seq {
        let minY = y - d - 1
        let maxY = y + d + 1
        if 0 <= minY && minY <= searchSpace then
            yield (x, minY)
        if 0 <= maxY && maxY <= searchSpace then
            yield (x, maxY)
        for i = (max 0 (y - d)) to min searchSpace (y + d) do
            let diffY = abs (y - i)
            let minX = x - d + diffY - 1
            let maxX = x + d - diffY + 1
            if 0 <= minX && minX <= searchSpace then
                yield (minX, i)
            if 0 <= maxX && maxX <= searchSpace then
                yield (maxX, i)
    }

let tryFreePointOnPerimeter (searchSpace: int) (sensors: SensorData[]) sensor =
    let rec loop space lst points =
        match Seq.tryHead points with
        | None -> None
        | Some point ->
            if Array.forall (fun s -> (dist s.Position point) > s.Radius) lst then
                Some point
            else
                loop space lst (Seq.tail points)

    let perimeterPoints = generatePerimeter searchSpace sensor
    printfn $"Looping {perimeterPoints |> Seq.length} perimeter points; {sensors.Length} sensors"
    loop searchSpace sensors perimeterPoints

let tryFindGap (searchSpace: int) (sensorData: SensorData[]) : (int * int) option =
    let rec loop space allSensors lst =
        match Array.tryHead lst with
        | None -> None
        | Some sensor ->
            let result = tryFreePointOnPerimeter space allSensors sensor
            match result with
            | Some p -> Some p
            | None -> loop space allSensors (Array.tail lst)
    
    loop searchSpace sensorData sensorData

let solvePart2StillSlow (searchSpace: int) (sensorData: SensorData array) : int =
    sensorData
    |> Array.map (fun s -> { s with Radius = dist s.Position s.Beacon })
    |> tryFindGap searchSpace
    |> function
        | Some(x, y) -> x * 4_000_000 + y
        | None -> failwith "No free point was found."

let getIntersections space sensorData =
    let minX = fst sensorData.Position - sensorData.Radius
    let maxX = fst sensorData.Position + sensorData.Radius
    let minY = snd sensorData.Position - sensorData.Radius
    let maxY = snd sensorData.Position + sensorData.Radius
    0

let solvePart2 (searchSpace: int) (sensorData: SensorData array) : int =
    sensorData
    |> Array.map (fun s -> { s with Radius = dist s.Position s.Beacon })
    |> getIntersections searchSpace
