module DijkstraMap

// dijkstra maps as described here http://www.roguebasin.com/index.php?title=The_Incredible_Power_of_Dijkstra_Maps
// see: https://gist.github.com/isobel-cullen/2910c8bf6bec1e45b37ad2051638c6e0

let flatten (A: 'a[,]) = A |> Seq.cast<'a>

let getColumn c (A: _[,]) = flatten A.[*, c..c] |> Seq.toArray

let getRow r (A: _[,]) = flatten A.[r..r, *] |> Seq.toArray

let displayGrid (grid: int[,]) : unit =
    let numRows = grid.GetLength 0
    // System.Console.Clear ()
    let mutable result = ""

    for r = 0 to numRows - 1 do
        let row = getRow r grid |> Array.fold (fun s i -> s + "\t" + i.ToString ()) ""
        result <- result + row + "\n"

    result <- (result.Replace ("\n\t", "\n"))[1..]

    printf $"{result}"

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

let inline private selectPoints (elev: char[,]) x y points =
    points
    |> Array.filter (fun struct (a, b) ->
        let elevationDifference = int elev[a, b] - int elev[x, y]
        elevationDifference = 0 || elevationDifference = 1)

let generate x y elevations goals =
    let ceiling = (x * y)
    let arena = Array2D.create x y ceiling

    for struct (gx, gy) in goals do
        arena[gx, gy] <- 0

    let displayArena () = displayGrid arena

    let initialSet =
        goals
        |> Seq.collect (fun struct (x, y) -> neighbourCoordsStruct x y arena |> selectPoints elevations x y)
        |> Seq.filter (fun struct (x, y) -> arena[x, y] = ceiling)
        |> Seq.toArray

    let isCeiling struct (x, y) = arena[x, y] = ceiling

    let nextItems level struct (x, y) =
        if arena[x, y] = ceiling then
            arena[x, y] <- level
            neighbourCoordsStruct x y arena |> selectPoints elevations x y
        else
            Array.empty

    let rec fanOut level (items: struct (int * int) array) =
        displayArena ()

        if items.Length > 0 then
            // TODO make items a FIFO queue of encountered. Add a 'visited' set or check arena cells that still have ceiling value to know which cells still need to be visited
            let toExplore = items |> Array.collect (nextItems level) |> Array.filter isCeiling
            fanOut (level + 1) toExplore

    initialSet |> fanOut 1
    arena
