module Day4

type Coordinates = {
    BoardId : int
    X : int
    Y : int
}

type Board = {
    Id: int
    Field: int[][]
}

type Win = {
    Coordinates: Coordinates
    Boards: Board[]
    Checked: Set<(int * int * int)>
}

type Bingo = {
    Numbers : int list
    Map : Map<int, Coordinates list>
    Boards: Board[]
    Checked: Set<(int * int * int)>
    Winners: Set<int>
    LastWinner: Win option
}


let private getCoordinates (board: Board) =
    board.Field
    |> Array.mapi(fun i row -> row |> Array.mapi (fun j item -> (item, {BoardId = board.Id; X = i; Y = j})))
    |> Array.collect id

let private parse (input: string[]) : Bingo =
    let numbers = input.[0].Split(',') |> Array.map(fun x -> x |> int) |> List.ofArray

    let boardsCount = (input.Length - 1) / 6

    let boards = input
                 |> Array.skip 1
                 |> Array.splitInto boardsCount
                 |> Array.map(fun x -> x
                                       |> Array.skip 1
                                       |> Array.map (fun y -> y.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
                                                              |> Array.map(fun z -> z |> int)))
                 |> Array.mapi(fun i board -> { Id = i; Field = board })

    let map = boards
              |> Array.collect (getCoordinates)
              |> Array.groupBy fst
              |> Array.map (fun (key, value) -> (key, value |> Array.map snd |> List.ofArray ))
              |> Map.ofArray

    {
        Numbers = numbers
        Map = map
        Boards = boards
        Checked = Set.empty
        Winners = Set.empty
        LastWinner = None
    }

let private checkWin (bingo: Bingo) (coordinates: Coordinates) : Win option =
    let indexes = Array.init 5 id
    let rowScore = indexes |> Array.fold (fun state index -> if bingo.Checked.Contains((coordinates.BoardId, coordinates.X, index)) then state + 1 else state) 0
    let columnScore = indexes |> Array.fold (fun state index -> if bingo.Checked.Contains((coordinates.BoardId, index, coordinates.Y)) then state + 1 else state) 0
    match (rowScore, columnScore) with
    | (5, _) | (_, 5) -> { Coordinates = coordinates; Boards = bingo.Boards; Checked = bingo.Checked } |> Some
    | _ -> None

let private calcWin (win : Win) : int =
    let boardId = win.Coordinates.BoardId
    let field = win.Boards.[boardId].Field
    let luckyNumber = field.[win.Coordinates.X].[win.Coordinates.Y]
    let indexes = Array.init 5 id
    let matchedSum = indexes |> Array.sumBy (fun i -> indexes |> Array.sumBy(fun j -> if win.Checked.Contains((boardId, i, j)) then 0 else field.[i].[j]))
    printfn $"NUMS %i{luckyNumber} %i{matchedSum}"
    luckyNumber * matchedSum

let private handleNumber(bingo: Bingo) (number : int) : (Bingo * Win list) =
    let coordinatesList = bingo.Map |> Map.find number
    coordinatesList
        |> List.fold (fun (b, state) coordinates ->
            let isWinner = b.Winners.Contains(coordinates.BoardId)
            let (boardId, x, y) = (coordinates.BoardId, coordinates.X, coordinates.Y)
            let board = b.Boards.[boardId]
            let b' = { b with Checked = b.Checked |> Set.add((boardId, x, y))}
            let win = checkWin b' coordinates

            match (isWinner, win) with
            | (_,  None) | (true,  _) -> (b', state)
            | (false, Some w) -> (b', state @ [w])
        ) (bingo, [])


let rec private play (bingo: Bingo) (place: int) : Win option =
    match bingo.Numbers with
    | [] ->
        bingo.LastWinner
    | H :: T ->
        let (bingo', ws) = handleNumber bingo H
        match ws with
        | [] -> play { bingo' with Numbers = T } place
        | winners ->
            match place with
            | 1 -> winners.Head |> Some
            | _ ->
                let ws' = winners |> Array.ofList
                let rest = place - winners.Length
                match rest with
                | min when min < 1 -> ws'.[place - 1] |> Some
                | _ ->
                    let total = ws' |> Array.fold (fun state winner -> state |> Set.add winner.Coordinates.BoardId) bingo'.Winners
                    let bingo'' = { bingo' with Numbers = T; Winners = total; LastWinner = ws' |> Array.tryLast }
                    play bingo'' rest

let private solve (bingo: Bingo) (place: int) : string =
    let win = play bingo place
    match win with
    | Some w ->
        let result = calcWin w
        result.ToString()
    | None -> "Something went wrong"

let solve1 (input: string[]) : string =
    let bingo = input |> parse
    solve bingo 1

let solve2 (input: string[]) : string =
    let bingo = input |> parse
    solve bingo bingo.Boards.Length