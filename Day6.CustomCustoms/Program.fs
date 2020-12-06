open System

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1