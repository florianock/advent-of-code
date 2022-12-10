module Day09

// --- Day 9: Rope Bridge ---

exception UnknownMoveError of string
exception IndexOutOfRangeError of string
exception DiffOutOfRangeError of string

type Point = { X: int; Y: int }
type Rope = { Knots: Point[] }

type State =
    { Rope: Rope
      SavedPositions: Set<Point> }

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split "\n"
    |> Array.map (fun s ->
        s.Split ' '
        |> function
            | a -> (a[0], a[1]))
    |> List.ofArray


let moveCloseTo target k : Point =
    let sign i =
        if i < 0 then -1
        elif i > 0 then 1
        else 0

    let doMove (diffX, diffY) p =
        if diffX = 0 then
            { X = p.X
              Y = p.Y + (1 * (sign diffY)) }
        elif diffY = 0 then
            { X = p.X + (1 * (sign diffX))
              Y = p.Y }
        else
            match (diffX, diffY) with
            | diffX, diffY when (sign diffX) = -1 && (sign diffY) = -1 -> { X = p.X - 1; Y = p.Y - 1 }
            | diffX, diffY when (sign diffX) = 1 && (sign diffY) = -1 -> { X = p.X + 1; Y = p.Y - 1 }
            | diffX, diffY when (sign diffX) = -1 && (sign diffY) = 1 -> { X = p.X - 1; Y = p.Y + 1 }
            | diffX, diffY when (sign diffX) = 1 && (sign diffY) = 1 -> { X = p.X + 1; Y = p.Y + 1 }
            | _ -> raise (DiffOutOfRangeError $"Unable to handle diff {(diffX, diffY)}")

    let diffX = target.X - k.X
    let diffY = target.Y - k.Y

    if (abs diffX) <= 1 && (abs diffY) <= 1 then
        k
    else
        doMove (diffX, diffY) k

let nextState (state: State) (direction: string) : State =
    let head :: rest = (state.Rope.Knots |> Array.toList)

    let newHead =
        match direction with
        | "U" -> { X = head.X; Y = head.Y + 1 }
        | "R" -> { X = head.X + 1; Y = head.Y }
        | "D" -> { X = head.X; Y = head.Y - 1 }
        | "L" -> { X = head.X - 1; Y = head.Y }
        | _ -> raise (UnknownMoveError $"Unknown move: ${direction}")

    let newTail =
        rest
        |> List.fold
            (fun (ks: Point[]) (k: Point) ->
                let target = ks |> Array.last

                [| (moveCloseTo target k) |] |> Array.append ks)
            [| newHead |]

    let newSavedPositions = state.SavedPositions |> Set.add (newTail |> Array.last)

    { Rope = { Knots = newTail }
      SavedPositions = newSavedPositions }

let move initialState (direction, numSteps) =
    let rec loop state dir count =
        match count with
        | 0 -> state
        | count when count > 0 ->
            let newState = nextState state dir
            loop newState dir (count - 1)
        | _ -> raise (IndexOutOfRangeError $"${nameof count} out of range: ${count}")

    let result = loop initialState direction (int numSteps)
    result

let solvePart1 (input: (string * string) list) =
    let startPositions = { Knots = [| { X = 0; Y = 0 }; { X = 0; Y = 0 } |] }

    let startState =
        { Rope = startPositions
          SavedPositions = Set.empty }

    let endState = input |> List.fold move startState

    endState.SavedPositions |> Set.count

let solvePart2 (input: (string * string) list) =
    let startPositions =
        { Knots =
            [| for _ in 0..9 do
                   yield { X = 0; Y = 0 } |] }

    let startState =
        { Rope = startPositions
          SavedPositions = Set.empty }

    let endState = input |> List.fold move startState

    endState.SavedPositions |> Set.count
