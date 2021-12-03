module Day2

type Command = {
    Name : string
    Value: int
}

type State1 = {
    Horizontal: int
    Depth: int
}

type State2 = {
    Horizontal: int
    Depth: int
    Aim: int
}

let private parse =
    Array.map (fun (x : string) ->
        let parts = x.Split(' ')
        { Name = parts.[0]; Value = parts.[1] |> int }
    )

let solve1 (input: string[]) : string =
    let commands = input |> parse
    let state = commands |> Array.fold (
                    fun state command ->
                        match (command.Name, command.Value) with
                        | ("up", up) -> { Horizontal = state.Horizontal; Depth = state.Depth - up }
                        | ("down", down) -> { Horizontal = state.Horizontal; Depth = state.Depth + down }
                        | ("forward", forward) -> { Horizontal = state.Horizontal + forward; Depth = state.Depth }
                        | _ -> state
                    ) {Horizontal = 0; Depth = 0}

    let result = state.Depth * state.Horizontal
    result.ToString()

let solve2 (input: string[]) : string =
    let commands = input |> parse
    let state = commands |> Array.fold (
                    fun state command ->
                        match (command.Name, command.Value) with
                        | ("up", up) -> { Horizontal = state.Horizontal; Depth = state.Depth; Aim = state.Aim - up }
                        | ("down", down) -> { Horizontal = state.Horizontal; Depth = state.Depth; Aim = state.Aim + down }
                        | ("forward", forward) -> { Horizontal = state.Horizontal + forward; Depth = state.Depth + state.Aim * forward; Aim = state.Aim }
                        | _ -> state
                    ) {Horizontal = 0; Depth = 0; Aim = 0}

    let result = state.Depth * state.Horizontal
    result.ToString()