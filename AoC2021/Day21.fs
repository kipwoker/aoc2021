module Day21

open Core

type Player = {
    Id: int
    Score: int
    Position: int
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

let turn player dice =
    let dice', offset = roll dice
    let pos = offset + player.Position
    let position' = ((pos - 1) % 10) + 1
    let score' = player.Score + position'
    let r = ({player with Position = position'; Score = score'}, dice')
    let (i,j) = r
    printfn $"%i{i.Id} score:%i{i.Score} pos:%i{i.Position} dice:%i{j}"
    r

let rec play (a: Player) (b:Player) dice counter =
    match (a,b) with
    | a', _ when a'.Score >= 1000 ->
        printfn $"%i{counter} %i{b.Score}"
        b.Score * counter * 3
    | _, b' when b'.Score >= 1000 ->
        printfn $"%i{counter} %i{a.Score}"
        a.Score * counter * 3
    | _ ->
        match (counter % 2) with
        | 0 ->
            let a', dice' = turn a dice
            play a' b dice' (counter + 1)
        | _ ->
            let b', dice' = turn b dice
            play a b' dice' (counter + 1)

let solve input =
    let aPosition, bPosition = input |> parse
    let a,b = ({Id = 1; Score = 0; Position = aPosition}, {Id = 2; Score = 0; Position = bPosition})
    printfn $"%A{(a,b)}"
    let result = play a b 0 0
    result.ToString()