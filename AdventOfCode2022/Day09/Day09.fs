module Day09

// --- Day 9: Rope Bridge ---

exception UnknownMoveError of string
exception IndexOutOfRangeError of string

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
    let diffX = target.X - k.X
    let diffY = target.Y - k.Y

    match (diffX, diffY) with
    | 0, 0 -> k
    | 1, 0 -> k
    | -1, 0 -> k
    | 0, 1 -> k
    | 0, -1 -> k
    | 1, 1 -> k
    | -1, -1 -> k
    | 2, 0 -> { X = k.X + 1; Y = k.Y }
    | -2, 0 -> { X = k.X - 1; Y = k.Y }
    | 0, 2 -> { X = k.X; Y = k.Y + 1 }
    | 0, -2 -> { X = k.X; Y = k.Y - 1 }
    | 2, 1 -> { X = k.X + 1; Y = k.Y + 1 }
    | -2, 1 -> { X = k.X - 1; Y = k.Y + 1 }
    | 2, -1 -> { X = k.X + 1; Y = k.Y - 1 }
    | -2, -1 -> { X = k.X - 1; Y = k.Y - 1 }
    | 1, 2 -> { X = k.X + 1; Y = k.Y + 1 }
    | -1, 2 -> { X = k.X - 1; Y = k.Y + 1 }
    | 1, -2 -> { X = k.X + 1; Y = k.Y - 1 }
    | -1, -2 -> { X = k.X - 1; Y = k.Y - 1 }
    | -2, -2 -> { X = k.X - 1; Y = k.Y - 1 }
    | 2, -2 -> { X = k.X + 1; Y = k.Y - 1 }
    | -2, 2 -> { X = k.X - 1; Y = k.Y + 1 }
    | 2, 2 -> { X = k.X + 1; Y = k.Y + 1 }
    | _ -> k

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
            [| { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 }
               { X = 0; Y = 0 } |] }

    let startState =
        { Rope = startPositions
          SavedPositions = Set.empty }

    let endState = input |> List.fold move startState

    endState.SavedPositions |> Set.count
