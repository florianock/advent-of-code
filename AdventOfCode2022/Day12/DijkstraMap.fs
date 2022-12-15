module DijkstraMap

// dijkstra maps as described here http://www.roguebasin.com/index.php?title=The_Incredible_Power_of_Dijkstra_Maps

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

let generate x y elevations goals =
    let ceiling = (x * y)
    let arena = Array2D.create x y ceiling

    for struct (gx, gy) in goals do
        arena.[gx, gy] <- 0

    let selectPoints (elev: char[,]) x y points =
        points
        |> Array.filter (fun struct (a, b) ->
            let elevationDifference = int elev[a, b] - int elev[x, y]
            elevationDifference = 0 || elevationDifference = 1)

    let initialSet =
        goals
        |> Seq.collect (fun struct (x, y) -> neighbourCoordsStruct x y arena |> selectPoints elevations x y)
        |> Seq.filter (fun struct (x, y) -> arena.[x, y] = ceiling)
        |> Seq.toArray

    let isCeiling struct (x, y) = arena.[x, y] = ceiling

    let nextItems level struct (x, y) =
        if arena[x, y] = ceiling then
            arena[x, y] <- level
            neighbourCoordsStruct x y arena |> selectPoints elevations x y
        else
            Array.empty

    let rec fanOut level (items: struct (int * int) array) =
        if items.Length > 0 then
            // TODO make items a FIFO queue of encountered. Add a 'visited' set or check arena cells that still have ceiling value to know which cells still need to be visited
            fanOut (level + 1) (items |> Array.collect (nextItems level) |> Array.filter isCeiling)

    initialSet |> fanOut 1
    arena
