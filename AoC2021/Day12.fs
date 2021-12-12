module Day12

open System

type State = Valid | Invalid

type Path = {
    State : State
    Nodes : string list
}

let parse (input : string[]) =
    let directions =
        input
        |> Array.map (fun line ->
            let parts = line.Split('-')
            (parts.[0], parts.[1])
        )
    let reverseDirections = directions |> Array.map ( fun (x,y) -> (y,x))

    Array.concat [directions ; reverseDirections]
    |> Array.filter (fun (x, y) -> x <> "end" && y <> "start")
    |> Array.groupBy fst
    |> Array.map (fun (x, y) -> (x, y |> Array.map snd))
    |> Map.ofArray

let traverseChildren routes node small maxSmall find' =
    routes
    |> Map.find node
    |> Array.map (fun destination ->
                        find' routes destination small maxSmall
                        |> List.map (fun path ->
                            {
                                State = path.State
                                Nodes = [node] @ path.Nodes
                            })
                  )
    |> Array.fold (fun state l -> state @ l) []

let rec find (routes: Map<string, string[]>) (node: string) (small: Set<string>) (maxSmall: int) : Path list =
    match node with
    | "start" ->
        traverseChildren routes node small maxSmall find
    | "end" ->
        [{
            State = Valid
            Nodes = ["end"]
        }]
    | n when Char.IsLower(n.[0]) ->
        let containsSmall = small.Contains(n)
        match (containsSmall, maxSmall) with
        | (true, 0) ->
            [{
                State = Invalid
                Nodes = [n]
            }]
        | (true, _) ->
            traverseChildren routes node (small.Add n) (maxSmall-1) find
        | (false, _) ->
            traverseChildren routes node (small.Add n) maxSmall find
    | _ ->
        traverseChildren routes node small maxSmall find


let solve (input: string[]) (maxSmall: int) : string =
    let routes = input |> parse
    let result = find routes "start" Set.empty maxSmall
                 |> List.fold (fun state el ->
                      state + (if el.State = Valid then 1 else 0)
                    ) 0
    result.ToString()

let solve1 input = solve input 0
let solve2 input = solve input 1