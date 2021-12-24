module Day23

open Core

type State = {
    // rooms first -> hallway last
    Rooms : int option[][]
    Score : int
    MinScore : int
    IterationsCount: int
}

let getLetter a =
    match a with
    | Some a' ->
        match a' with
        | 1 -> "A"
        | 10 -> "B"
        | 100 -> "C"
        | 1000 -> "D"
        | _ ->
            printfn $"Unknown %i{a'}"
            "ERROR"
    | None -> "."

let print state =
    let hallwayLength = state.Rooms.[4].Length
    for i = 1 to hallwayLength + 2 do
        printf "#"
    printfn ""
    printf "#"
    for i = 0 to hallwayLength - 1 do
        let letter = getLetter state.Rooms.[4].[i]
        printf $"%s{letter}"
    printf "#"
    printfn ""
    for i = 0 to state.Rooms.[0].Length - 1 do
        printf "##|"
        printf $"%s{getLetter state.Rooms.[0].[i]}"
        printf "|"
        printf $"%s{getLetter state.Rooms.[1].[i]}"
        printf "|"
        printf $"%s{getLetter state.Rooms.[2].[i]}"
        printf "|"
        printf $"%s{getLetter state.Rooms.[3].[i]}"
        printf "|##"
        printfn ""
    printfn ""
    printfn $"Score = %i{state.Score}; MinScore = %A{state.MinScore}"

let convert a =
    match a with
    | 'A' -> 1
    | 'B' -> 10
    | 'C' -> 100
    | 'D' -> 1000
    | _ ->
        printfn $"Unknown %c{a}"
        0
    |> Some

let prod = {
    Rooms = [|
        [|'B'; 'D'; 'D'; 'C'|] |> Array.map convert
        [|'D'; 'C'; 'B'; 'D'|] |> Array.map convert
        [|'C'; 'B'; 'A'; 'B'|] |> Array.map convert
        [|'A'; 'A'; 'C'; 'A'|] |> Array.map convert
        Array.init 11 (fun _ -> None)
    |]
    Score = 0
    MinScore = 900000
    IterationsCount = 0
}

let sumOrDie lst =
    lst
    |> List.fold (fun sum x ->
        match sum, x() with
        | None, _ -> None
        | _, None -> None
        | Some s, Some x' -> s + x' |> Some
    ) (Some 0)

let isDoor x = x = 2 || x = 4 || x = 6 || x = 8

let getTargetRoom weight =
    match weight with
    | 1 -> 0
    | 10 -> 1
    | 100 -> 2
    | 1000 -> 3
    | _ ->
        printfn $"Unknown %i{weight}"
        -1

let getHallIndex roomIndex =
    match roomIndex with
    | 0 -> 2
    | 1 -> 4
    | 2 -> 6
    | 3 -> 8
    | _ ->
        printfn $"Unknown %i{roomIndex}"
        -1

let hasObstacle state offset length = Array.sub state.Rooms.[4] offset length |> Array.exists(fun x -> x.IsSome)

let getCostInHallway state y1 y2 p =
    let min' = min y1 y2
    let max' = max y1 y2
    let length = max' - min' + 1
    let hasObstacle' = hasObstacle state min' length
    if hasObstacle' then
        None
    else
        p * length |> Some

let getLeaveRoomCost state x1 y1 p =
    let isBusy() = state.Rooms.[x1] |> Array.take y1 |> Array.exists (fun x -> x.IsSome)
    let target = getTargetRoom p
    let isAlreadyThere() = state.Rooms.[x1] |> Array.exists (fun x -> x.IsSome && x.Value <> target) |> not

    if isBusy() || isAlreadyThere() then
        None
    else
        y1 * p |> Some

let getResideInTheRoomCost state target p =
    let emptyPlaces = state.Rooms.[target] |> Array.takeWhile (fun c -> c.IsNone)
    let wrongCells = state.Rooms.[target] |> Array.skipWhile (fun c -> c.IsNone) |> Array.filter (fun c -> c.IsNone || target <> getTargetRoom c.Value)
    if emptyPlaces.Length = 0 || wrongCells.Length = 0 then
        None
    else
        emptyPlaces.Length * p |> Some

let getCostBetweenRooms state x1 y1 x2 p =
    let target = getTargetRoom p
    if x2 <> target then
        None
    else
        let leaveRoomCost() = getLeaveRoomCost state x1 y1 p
        let startHallIndex = getHallIndex x1
        let endHallIndex = getHallIndex x2
        let hallwayWalkCost() = getCostInHallway state startHallIndex endHallIndex p
        let resideInTheRoomCost() = getResideInTheRoomCost state target p
        sumOrDie [hallwayWalkCost; resideInTheRoomCost; leaveRoomCost]

let getCost state x1 y1 x2 y2 =
    if (x1 = x2 && x1 <> 4) || (x1 = x2 && y1 = y2) then
        None
    else
        let weight = state.Rooms.[x1].[y1]
        match weight with
        | None -> None
        | Some weight' ->
            match x1 with
            | 4 ->
                match x2 with
                | 4 ->
                    if isDoor y2 then
                        None
                    else
                        getCostInHallway state y1 y2 weight'
                | _ ->
                    let hallwayIndex = getHallIndex x2
                    let hallwayWalkCost() = getCostInHallway state y1 hallwayIndex weight'
                    let resideInTheRoomCost() = getResideInTheRoomCost state x2 weight'
                    sumOrDie [hallwayWalkCost; resideInTheRoomCost]
            | _ ->
                match x2 with
                | 4 ->
                    let leaveRoomCost() = getLeaveRoomCost state x1 y1 weight'
                    let hallwayIndex = getHallIndex x1
                    if isDoor y2 then
                        None
                    else
                        let hallwayWalkCost() = getCostInHallway state hallwayIndex y2 weight'
                        sumOrDie [leaveRoomCost; hallwayWalkCost]
                | _ -> getCostBetweenRooms state x1 y1 x2 weight'

let copy rooms = rooms |> Array.map Array.copy

let isWin state =
    let full = state.Rooms
                |> Array.take 4
                |> Array.mapi (fun i r -> r |> Array.filter (fun x -> x.IsSome && getTargetRoom x.Value = i) |> Array.length)
                |> Array.filter (fun x -> x = 4)
    full.Length = 4

let rec calculateTurns (state : State) : State option =
    if state.IterationsCount >= 30 then
        None
    elif isWin state then
        { state with MinScore = min state.MinScore state.Score } |> Some
    else
        state.Rooms
        |> Array.mapi (fun i _ ->
            state.Rooms.[i]
            |> Array.mapi (fun j w ->
                match w with
                | None -> [||]
                | Some _ ->
                    state.Rooms
                    |> Array.mapi (fun x _ ->
                        state.Rooms.[x]
                        |> Array.mapi (fun y _ -> (x, y, getCost state i j x y)
                        )
                        |> Array.filter (fun (_, _, cost') -> cost'.IsSome)
                        |> Array.map (fun (x',y', cost') -> (x',y',cost'.Value))
                    )
                    |> Array.collect id
                    |> Array.sortBy (fun (_,_, cost') -> cost')
                    |> Array.map(fun (x', y', cost') ->
                            let rooms = copy state.Rooms
                            let item = rooms.[i].[j]
                            rooms.[i].[j] <- None
                            rooms.[x'].[y'] <- item
                            let state' = { state with Rooms = rooms; Score = state.Score + cost'; MinScore = state.MinScore }
                            calculateTurns state'
                        )
            )
            |> Array.collect id
        )
        |> Array.collect id
        |> Array.append [| None |]
        |> Array.minBy (fun t -> if t.IsNone then 900000 else t.Value.MinScore)



let solve _ =
    print prod
    let state' = calculateTurns prod
    let result = state'
    if state'.IsSome then
        print state'.Value
    result.ToString()