module Day09

exception UnknownMoveError of string
exception IndexOutOfRangeError of string

type Point = { X: int; Y: int }
type Rope = { Head: Point; Tail: Point }

type State =
    { Rope: Rope
      SavedPositions: Set<Point> }

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split ("\n")
    |> Array.map (fun s ->
        s.Split (' ')
        |> function
            | a -> (a[0], a[1]))
    |> List.ofArray

let nextState (state: State) (direction: string) : State =
    let currentX = state.Rope.Head.X
    let currentY = state.Rope.Head.Y

    let newHead =
        match direction with
        | "U" -> { X = currentX; Y = currentY + 1 }
        | "R" -> { X = currentX + 1; Y = currentY }
        | "D" -> { X = currentX; Y = currentY - 1 }
        | "L" -> { X = currentX - 1; Y = currentY }
        | _ -> raise (UnknownMoveError $"Unknown move: ${direction}")

    let distX = abs (newHead.X - state.Rope.Tail.X)
    let distY = abs (newHead.Y - state.Rope.Tail.Y)

    let newTail =
        if distX > 1 || distY > 1 then
            { X = currentX; Y = currentY }
        else
            state.Rope.Tail

    let newSavedPositions = state.SavedPositions |> Set.add newTail

    { Rope = { Head = newHead; Tail = newTail }
      SavedPositions = newSavedPositions }


let move state (direction, numSteps) =
    let rec loop st dir count =
        match count with
        | 0 -> st
        | count when count > 0 ->
            let newState = nextState st dir
            loop newState dir (count - 1)
        | _ -> raise (IndexOutOfRangeError $"${nameof (count)} out of range: ${count}")

    loop state direction (int numSteps)

let solvePart1 (input: (string * string) list) =
    let startPositions =
        { Head = { X = 0; Y = 0 }
          Tail = { X = 0; Y = 0 } }

    let startState =
        { Rope = startPositions
          SavedPositions = Set.empty }

    let endState = input |> List.fold move startState

    endState.SavedPositions |> Set.count

let solvePart2 (input: (string * string) list) = 0
