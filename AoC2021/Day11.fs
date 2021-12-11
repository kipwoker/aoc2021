module Day11

type Octo = {
    Level: int
    Flashed: bool
}

let inline charToInt c = int c - int '0'

let private parse (input: string[]) =
    let a = input
            |> Array.map (fun x -> x.ToCharArray() |> Array.map (fun y -> y |> charToInt))
    Array2D.init 10 10 (fun x y -> { Level = a.[x].[y]; Flashed = false })

let flash a x y inc =
    for i = -1 to 1 do
        for j = -1 to 1 do
            let dx = x + i
            let dy = y + j
            match (dx, dy) with
            | (-1, _) | (_, -1) | (10, _) | (_, 10) -> ignore 0
            | (xx,yy) when xx = x && yy = y -> ignore 0
            | (x', y') -> inc a x' y'

let rec increase (a : Octo[,]) i j =
    let current = a.[i,j]
    match current.Flashed with
    | true -> ignore 0
    | false ->
        let update = {
            Level = (current.Level + 1) % 10
            Flashed = (current.Level + 1) = 10
        }
        a.[i,j] <- update
        match update.Flashed with
        | true -> flash a i j increase
        | false -> ignore 0

let makeStep (a: Octo[,]) =
    for i = 0 to 9 do
        for j = 0 to 9 do
            increase a i j

let reset (a: Octo[,]) =
    for i = 0 to 9 do
        for j = 0 to 9 do
            let current = a.[i,j]
            let update = {
                Level = current.Level
                Flashed = false
            }
            a.[i,j] <- update

let calcFlashed (a: Octo[,]) =
    let mutable sum = 0
    for i = 0 to 9 do
        for j = 0 to 9 do
            if a.[i,j].Flashed then
                sum <- sum + 1
    sum

let play (a: Octo[,]) (count : int) =
    [1 .. count]
    |> List.fold (fun state step ->
        makeStep a
        let count = calcFlashed a
        reset a
        count + state
       ) 0

let solve1 (input: string[]) : string =
    let octos = input |> parse
    let result = play octos 100
    result.ToString()

let checkSync (a: Octo[,]) =
    let mutable sum = 0
    for i = 0 to 9 do
        for j = 0 to 9 do
            sum <- sum + a.[i,j].Level
    sum = 0

let sync (a: Octo[,]) =
    let mutable proceed = true
    let mutable counter = 0
    while proceed do
        counter <- counter + 1
        makeStep a
        proceed <- (checkSync a) |> not
        reset a
    counter

let solve2 (input: string[]) : string =
    let octos = input |> parse
    let result = octos |> sync
    result.ToString()