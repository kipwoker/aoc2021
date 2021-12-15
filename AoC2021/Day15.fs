module Day15

let inline charToInt c = int c - int '0'

let parse (input: string[]) =
    input |> Array.map (fun x -> x.ToCharArray() |> Array.map charToInt)

let getGrid m n i j =
    [
        if i <= 0 then [] else [(i - 1, j)]
        if j <= 0 then [] else [(i, j - 1)]
        if i >= m - 1 then [] else [(i + 1, j)]
        if j >= n - 1 then [] else [(i, j + 1)]
    ] |> List.collect id

let calcRisk current new' =
    match current with
    | None -> new'
    | Some c -> min c new'
    |> Some

let rec dijkstra (a : int[][]) m n (queue: (int*int) list) (risk : int option[,]) (visited: Set<(int * int)> ref) (parents : Map<(int*int), (int*int)> ref) =
    if queue.IsEmpty then
        ignore 0
    else
        let queue' = queue |>
                        List.map (fun (i,j) ->
                                if visited.Value |> Set.contains (i,j) then
                                    []
                                else
                                    let grid = getGrid m n i j
                                    let r = risk.[i,j].Value
                                    grid |> List.iter (fun (x,y) ->
                                            let current = risk.[x,y]
                                            let new' = r + a.[x].[y]
                                            if current.IsNone || current.Value > new' then
                                                risk.[x,y] <- Some new'
                                                parents := !parents |> Map.change (x,y) (fun _ -> (i,j) |> Some)
                                        )
                                    visited := Set.add (i, j) !visited
                                    grid
                            )
                        |> List.collect id
        dijkstra a m n queue' risk visited parents

let rec getPath (parents: Map<(int*int), (int*int)>) (i,j) : (int*int) list =
    match (i,j) with
    | (0,0) -> [(i,j)]
    | _ ->
        let (x,y) = parents |> Map.find (i,j)
        getPath parents (x,y) @ [(i,j)]

let traverse (matrix : int[][]) =
    let m = matrix.Length
    let n = matrix.[0].Length
    let risk = Array2D.init<int option> m n (fun _ _ -> None)
    risk.[0, 0] <- Some 0
    let visited = ref Set.empty
    let parents = ref Map.empty
    dijkstra matrix m n [0, 0] risk visited parents

    risk.[m-1, n-1].Value

let clone count (a: int[][]) =
    let m = a.Length
    let n = a.[0].Length
    let m' = m * count
    let n' = n * count
    Array.init m' (fun i -> Array.init n' (fun j ->
        let el = i /m + j / n + a.[i % m].[j % n]
        if el >= 10 then (el % 10) + 1 else el
        )
    )

let solve1 input =
    let result = input |> parse |> traverse
    result.ToString()

let solve2 input =
    let result = input |> parse |> clone 5 |> traverse
    result.ToString()