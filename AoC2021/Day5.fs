module Day5

type Point = {
    X: int
    Y: int
}

type Line = {
    Start: Point
    End: Point
}

let private parsePoint(input : string) : Point =
    let coordinates = input.Split([| ',' |])
    {
        X = coordinates.[1] |> int
        Y = coordinates.[0] |> int
    }

let private parse(input: string[]) : Line[] =
    input
    |> Array.map
           (fun i ->
                let parts = i.Split([| " -> " |], System.StringSplitOptions.None)
                {
                    Start = parts.[0] |> parsePoint
                    End = parts.[1] |> parsePoint
                }
            )

let private getMax (lines: Line[]) : (int * int) =
    let maxX = lines |> Array.map(fun line -> max line.Start.X line.End.X) |> Array.max
    let maxY = lines |> Array.map(fun line -> max line.Start.Y line.End.Y) |> Array.max
    (maxX, maxY)

let private initField (maxX, maxY) = Array2D.init (maxX + 1) (maxY + 1) (fun _ _ -> 0)

let private applyLines (field: int[,]) (lines: Line[]) (diagonal: bool) : int[,] =
    lines |> Array.iter (fun line ->
        match (line.End.X = line.Start.X, line.End.Y = line.Start.Y, diagonal) with
        | (true, _, _) ->
            let min' = min line.Start.Y line.End.Y
            let max' = max line.Start.Y line.End.Y
            let count = max' - min' + 1
            let x = line.Start.X
            Array.init count id
            |> Array.iter (fun i ->
                let y = min' + i
                let value = field.[x, y]
                Array2D.set field x y (value + 1)
            )
        | (_, true, _) ->
            let min' = min line.Start.X line.End.X
            let max' = max line.Start.X line.End.X
            let count = max' - min' + 1
            let y = line.Start.Y
            Array.init count id
            |> Array.iter (fun i ->
                let x = min' + i
                let value = field.[x, y]
                Array2D.set field x y (value + 1)
            )
        | (_, _, true) ->
            let minX = min line.Start.X line.End.X
            let maxX = max line.Start.X line.End.X
            let minY = min line.Start.Y line.End.Y
            let maxY = max line.Start.Y line.End.Y

            let countX = maxX - minX + 1
            let countY = maxY - minY + 1
            if countX = countY then
                let count = countX
                let sy = line.Start.Y
                let sx = line.Start.X
                let ey = line.End.Y
                let ex = line.End.X
                Array.init count id
                |> Array.iter (fun i ->
                    let dx = if sx > ex then -1 else 1
                    let dy = if sy > ey then -1 else 1
                    let x = sx + dx * i
                    let y = sy + dy * i
                    let value = field.[x, y]
                    Array2D.set field x y (value + 1)
                )
            else
                ignore 0
        | (false, false, false) -> ignore 0
    )
    field

let private count (field: int[,]) (moreThan: int) : int =
    field
    |> Seq.cast<int>
    |> Seq.filter (fun x -> x > moreThan)
    |> Seq.sumBy (fun _ -> 1)

let solve (input: string[]) (applyDiagonal: bool) : string =
    let lines = input |> parse
    let (maxX, maxY) =  lines |> getMax
    let field = initField (maxX, maxY)
    let field' = applyLines field lines applyDiagonal
    printfn $"%A{field'}"
    let result = count field' 1
    result.ToString()

let solve1 input = solve input false
let solve2 input = solve input true