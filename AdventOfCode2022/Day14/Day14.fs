module Day14


// --- Day 14: Regolith Reservoir ---

exception UnknownFormatError of string

type Line = int * int

type Row = Line list

type Field = Row list

type State =
    { Start: int * int
      Field: Field
      GrainsOfSand: int }

let preprocess (puzzle: string) : Line list list =
    let rocks =
        puzzle.TrimEnd().Split "\n"
        |> Seq.map (fun s ->
            s.Split " -> "
            |> Array.map (fun p ->
                p.Split ","
                |> Array.toList
                |> function
                    | [ x; y ] -> (int x, int y)
                    | p -> raise (UnknownFormatError $"Unknown format for point {p}"))
            |> List.ofArray)
        |> List.ofSeq

    rocks
    |> List.collect (fun r ->
        r
        |> List.pairwise
        |> List.collect (fun ((s1, e1), (s2, e2)) ->
            if s1 = s2 then
                let es = [ e1..e2 ]

                List.zip
                    [ for i = 0 to es.Length - 1 do
                          s1 ]
                    [ e1..e2 ]
            else
                let ss = [ s1..s2 ]

                List.zip
                    [ s1..s2 ]
                    [ for i = 0 to ss.Length - 1 do
                          e1 ]))

let inline lineContains i (x, y) = x <= i && i <= y
let inline rowContains i row = List.exists (lineContains i) row
// TODO sort row -> merge lines if necessary
let fitRow row =
    let rec mergeLines r =
        let sorted = r |> List.sortWith (fun (s1, e1) (s2, e2) -> compare s1 s2)

        let merged =
            sorted
            |> List.pairwise
            |> List.collect (fun pair ->
                match pair with
                | (s1, e1), (s2, e2) when e1 = s2 || s2 - e1 = 1 || e1 > s2 -> [ ((s1, e2), [ (s1, e1); (s2, e2) ]) ]
                | (s1, e1), (s2, e2) -> [])

        let mergedLines, toRemove = merged |> List.unzip

        let result =
            sorted |> List.except (toRemove |> List.concat) |> List.append mergedLines

        if result = r then result else mergeLines result

    mergeLines row


let addToField (x: int, y: int) (field: Field) : Field =
    field
    |> List.mapi (fun i r ->
        if i <> y then
            r
        else
            let withinRange = r |> List.exists (lineContains x)

            if withinRange then
                r // point falls within range of existing lines
            else
                fitRow (r |> List.append [ (x, x) ]))

let rollSandGrain (state: State) : (int * int) option =
    let rec go (x, y) (f: Field) =
        let maxY = f.Length - 1

        if y < maxY then
            let newRow = f[y + 1]

            if rowContains x newRow then
                if rowContains (x - 1) newRow then
                    if rowContains (x + 1) newRow then
                        (x, y) |> Some // has settled in position
                    else
                        go (x + 1, y + 1) f // continues down-right
                else
                    go (x - 1, y + 1) f // continues down-left
            else
                go (x, y + 1) f // continues down
        else
            None // into the void

    go state.Start state.Field

let simulateSandFalling (state: State) : State =
    let newPosOption = rollSandGrain state

    match newPosOption with
    | Some (x, y) ->
        { GrainsOfSand = state.GrainsOfSand + 1
          Field = addToField (x, y) state.Field
          Start = state.Start }
    | None -> state

let runUntilStable start field =
    let rec go (s: State) : State =
        let mutable newState = simulateSandFalling s

        while newState.GrainsOfSand <> s.GrainsOfSand do
            newState <- go newState

        newState

    go
        { Start = start
          Field = field
          GrainsOfSand = 0 }

let solvePart1 (field: Field) : int =
    let start = (500, 0)
    let endState = runUntilStable start field
    endState.GrainsOfSand

let solvePart2 (field: Field) : int = 0
