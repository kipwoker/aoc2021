module Day21

open Core

type Player = {
    Id: int
    Score: int
    Position: int
}

type Result1 = {
    Loser: Player
    IterationsCount: int
}

type Group = {
    Sum: int
    Count: bigint
}

let parse (input: string[]) =
    let startPoints = input.[0].Split(' ') |> Array.map int
    (startPoints.[0], startPoints.[1])

let roll' dice =
    match dice with
    | 100 -> 1
    | _ -> dice + 1

let roll dice =
    let dice' = roll' dice
    let dice'' = roll' dice'
    let dice''' = roll' dice''
    (dice''', dice' + dice'' + dice''')

let normalize position = ((position - 1) % 10) + 1

let changePosition player offset =
    let pos = offset + player.Position
    let position' = normalize pos
    let score' = player.Score + position'
    {player with Position = position'; Score = score'}

let turn player dice =
    let dice', offset = roll dice
    (changePosition player offset, dice')

let rec play1 (a: Player) (b:Player) winScore dice counter =
    match (a,b) with
    | a', _ when a'.Score >= winScore -> { Loser = b; IterationsCount = counter }
    | _, b' when b'.Score >= winScore -> { Loser = a; IterationsCount = counter }
    | _ ->
        match (counter % 2) with
        | 0 ->
            let a', dice' = turn a dice
            play1 a' b winScore dice' (counter + 1)
        | _ ->
            let b', dice' = turn b dice
            play1 a b' winScore dice' (counter + 1)

let calc1 result = result.Loser.Score * result.IterationsCount * 3

let getKey a b counter = (a.Position, b.Position, a.Score, b.Score, counter)

let print' a = "Id: " + a.Id.ToString() + " Pos: " + a.Position.ToString() + " Score: " + a.Score.ToString()
let print (a: Player) (b:Player) counter =
    printfn $"%s{print' a} || %s{print' b} || %i{counter}"

let zero = bigint 0
let one = bigint 1

let inline inc (x,y) =
    let inc' t = if t = zero then zero else t + one
    (inc' x, inc' y)

let rec play2 (a: Player) (b:Player) winScore cache counter (combinations : Group list) =
    let cacheKey = getKey a b counter
    match !cache |> Map.tryFind cacheKey with
    | Some result -> result
    | None ->
        match (a,b) with
        | a', _ when a'.Score >= winScore -> (one, zero)
        | _, b' when b'.Score >= winScore -> (zero, one)
        | _ ->
            let isFirst = counter % 2 = 0
            let result = combinations
                         |> List.map (fun g ->
                             let sum = g.Sum
                             let count = g.Count

                             let ar,br = if isFirst then
                                            play2 (changePosition a sum) b winScore cache (counter + 1) combinations
                                         else
                                            play2 a (changePosition b sum) winScore cache (counter + 1) combinations
                             (ar * count, br * count)
                         )
                         |> List.fold (fun (a',b') (x,y) -> (a' + x, b' + y)) (zero, zero)
            cache := Map.add cacheKey result !cache
            result

let calc2 (a,b) = max a b

let solve input =
    let aPosition, bPosition = input |> parse
    let a,b = ({Id = 1; Score = 0; Position = aPosition}, {Id = 2; Score = 0; Position = bPosition})
    let task1 = play1 a b 1000 0 0 |> calc1
    let cache = ref Map.empty
    let combinations = combinations 1 3 3
                       |> List.groupBy (fun x -> x |> List.sum)
                       |> List.map (fun (x,y) -> {Sum = x; Count = y.Length |> bigint})
    printfn $"%A{combinations}"
    let task2 = play2 a b 21 cache 0 combinations |> calc2
    let result = (task1, task2)
    result.ToString()