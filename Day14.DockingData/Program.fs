open System

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1