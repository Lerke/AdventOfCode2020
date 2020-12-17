open System

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 15 - Rambunctious Recitation"
        printfn "Path: %s" path
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
        