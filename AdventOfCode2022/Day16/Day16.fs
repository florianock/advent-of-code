module Day16

open System.Text.RegularExpressions

// --- Day 16: Proboscidea Volcanium ---

type ValveInput =
    { Name: string
      FlowRate: int
      Connects: string list }

type ValveStatus =
    | Open
    | Closed

type Valve =
    { Id: int
      FlowRate: int
      mutable Status: ValveStatus }

type State =
    { Valves: Valve list
      Labels: Map<int, string>
      Adjacency: bool[,]
      mutable RemainingMinutes: int
      mutable ReleasedPressure: int
      mutable CurrentValve: int }

let initializeState inputValves =
    let labels = inputValves |> List.mapi (fun i v -> (i, v.Name)) |> Map
    let reverseLabels = inputValves |> List.mapi (fun i v -> (v.Name, i)) |> Map

    let newValves =
        inputValves
        |> List.mapi (fun i v ->
            { Id = i
              FlowRate = v.FlowRate
              Status = Closed })

    let adjacency = Array2D.create inputValves.Length inputValves.Length false

    for v in inputValves do
        for c in v.Connects do
            adjacency.SetValue(true, reverseLabels[v.Name], reverseLabels[c])

    { Valves = newValves
      Labels = labels
      Adjacency = adjacency
      RemainingMinutes = 30
      ReleasedPressure = 0
      CurrentValve = 0 }

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split("\n")
    |> Array.map (fun l ->
        let r =
            Regex.Match(l, "Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([\w,\s]+)")

        { Name = r.Groups[1].Value
          FlowRate = int r.Groups[2].Value
          Connects = r.Groups[ 3 ].Value.Split(", ") |> List.ofArray })
    |> List.ofArray
    |> initializeState

let goalValves valves =
    valves
    |> List.filter (fun v -> v.Status = Closed && v.FlowRate > 0)
    |> List.sortByDescending (fun v -> v.FlowRate)

let act state minute =
    printfn $"== Minute {minute} =="

    // calculate pressure
    let pressureReleased =
        state.Valves
        |> List.filter (fun v -> v.Status = Open)
        |> List.map (fun v -> v.FlowRate)
        |> List.fold (+) 0

    let openValvesMessage =
        state.Valves
        |> List.filter (fun v -> v.Status = Open)
        |> List.map (fun v -> state.Labels[v.Id])
        |> (String.concat ", ")
    printfn $"Valves {openValvesMessage} are open, releasing {pressureReleased} pressure."

    state.ReleasedPressure <- state.ReleasedPressure + pressureReleased

    // open valve
    state.Valves[state.CurrentValve].Status <- Open
    printfn $"You open valve {state.Labels[state.CurrentValve]}."

    // move & strategy
    let goals = goalValves state.Valves
    let nextValve = goals.Head
    state.CurrentValve <- nextValve.Id
    printfn $"You move to valve {state.Labels[nextValve.Id]}."

    let moveOptions =
        state.Adjacency[state.CurrentValve, *]
        |> Array.mapi (fun i v -> if v then i else -1)
        |> Array.filter (fun v -> v >= 0)

    // countdown
    state.RemainingMinutes <- state.RemainingMinutes - 1
    state

let solvePart1 (state: State) = [ 1..30 ] |> List.fold act state |> (fun s -> s.ReleasedPressure)

let solvePart2 state = 0
