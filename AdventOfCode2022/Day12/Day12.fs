module Day12

// --- Day 12: Hill Climbing Algorithm ---

open Microsoft.FSharp.Collections

let START = 'S'
let END = 'E'

// type NodePaths =
//     { Node: string
//       End: string
//       Paths: Map<string, int> }
//
//     static member Start =
//         { Node = ""
//           End = ""
//           Paths = Map.empty }
//
//     member this.updatePath(k, v) : NodePaths =
//         let currentDistance =
//             if this.Paths.ContainsKey k then
//                 this.Paths[k]
//             else
//                 System.Int32.MaxValue
//
//         let newDistance = v + this.Paths[this.Node]
//         this

let inline private neighbourCoordsStruct x y arr =
    let boundsX = (Array2D.length1 arr) - 1
    let boundsY = (Array2D.length2 arr) - 1

    match x, y with
    | 0, 0 -> [| struct (1, 0); struct (0, 1) |]
    | x, y when x = boundsX && y = boundsY -> [| struct (x - 1, y); struct (x, y - 1) |]
    | 0, y when y = boundsY -> [| struct (1, y); struct (0, y - 1) |]
    | x, 0 when x = boundsX -> [| struct (x - 1, 0); struct (x, 1) |]
    | 0, y -> [| struct (0, y - 1); struct (0, y + 1); struct (1, y) |]
    | x, 0 -> [| struct (x, 1); struct (x + 1, 0); struct (x - 1, 0) |]
    | x, y when y = boundsY -> [| struct (x, y - 1); struct (x + 1, y); struct (x - 1, y) |]
    | x, y when x = boundsX -> [| struct (x, y - 1); struct (x, y + 1); struct (x - 1, y) |]
    | x, y -> [| struct (x, y - 1); struct (x, y + 1); struct (x + 1, y); struct (x - 1, y) |]

let preprocess (puzzle: string) : char[,] =
    puzzle.TrimEnd().Split "\n" |> Array.map (fun s -> s |> Seq.toArray) |> array2D

// type Graph (paths: int[,][,]) =
//     let weightedPaths = paths
//
//     let mutable evaluatedPaths: int[,,,] =
//         Array4D.create
//             (weightedPaths |> Array2D.length1)
//             (weightedPaths |> Array2D.length2)
//             (weightedPaths |> Array2D.length1)
//             (weightedPaths |> Array2D.length2)
//             System.Int32.MaxValue
//
//     member this.updateShortestPath(node: int * int) : int * int =
//         let subGraph = weightedPaths[node]
//
//         subGraph
//         |> Array2D.iteri (fun a b c ->
//             let currentDistance =
//                 evaluatedPaths.TryFind (a, b)
//                 |> function
//                     | Some i -> i
//                     | None -> System.Int32.MaxValue
//
//             let existingDistance =
//                 evaluatedPaths.TryFind node
//                 |> function
//                     | Some i -> i
//                     | None -> 0
//
//             let newDistance = c + existingDistance
//
//             evaluatedPaths <-
//                 if newDistance < currentDistance then
//                     evaluatedPaths.Add ((a, b), newDistance)
//                 else
//                     evaluatedPaths)
//
//         evaluatedPaths <- evaluatedPaths.Remove node
//         let minDistance = evaluatedPaths.Values |> Seq.min
//         evaluatedPaths |> Map.findKey (fun _ v -> v = minDistance)
//
//     member this.findShortestPath (startValue: int * int) (endValue: int * int) : int =
//         let mutable current = startValue
//
//         while current <> endValue do
//             current <- this.updateShortestPath current
//
//         evaluatedPaths |> Map.find endValue

let find2D (c: char) (arr: char[,]) : (int * int) option =
    let rec go x y =
        if y >= arr.GetLength 1 then None
        elif x >= arr.GetLength 0 then go 0 (y + 1)
        elif arr[x, y] = c then Some (x, y)
        else go (x + 1) y

    go 0 0

let flatten (A: 'a[,]) = A |> Seq.cast<'a>

let getColumn c (A: _[,]) = flatten A.[*, c..c] |> Seq.toArray

let getRow r (A: _[,]) = flatten A.[r..r, *] |> Seq.toArray

let displayGrid (grid: int[,]) : unit =
    let numRows = grid.GetLength 0
    let mutable result = "\n"

    for r = 0 to numRows - 1 do
        let row = getRow r grid |> Array.fold (fun s i -> s + "\t" + i.ToString ()) ""
        result <- result + row + "\n"

    result <- result.Replace ("\n\t", "\n")

    printf $"{result}"

let solvePart1 (input: char[,]) =
    let sp = input |> find2D START |> Option.fold (fun _ v -> v) (-1, -1)
    let s1, s2 = sp
    let startPos = struct (s1, s2)
    let e1, e2 = input |> find2D END |> Option.fold (fun _ v -> v) (-1, -1)
    Array2D.set input s1 s2 (char ((int 'a') - 1))
    Array2D.set input e1 e2 (char ((int 'z') + 1))

    let length1 = input |> Array2D.length1
    let length2 = input |> Array2D.length2

    let result = DijkstraMap.generate length1 length2 input [ startPos ]

    // let mutable startPaths = Map.empty
    //
    // input
    // |> Array2D.iteri (fun x y c -> startPaths <- startPaths.Add ((x, y), Map [ ((x, y), Map.empty) ]))

    // let graph = Graph startPaths
    // graph.findShortestPath startPos endPos
    // displayGrid result
    result[e1, e2]
// 3403 is too high

let solvePart2 (input: char[,]) = 0
