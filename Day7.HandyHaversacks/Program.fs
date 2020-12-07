open System
open System.IO

[<EntryPoint>]
let main argv =
    match Array.tryHead argv with
    | Some path ->
        printfn "Day 7 - Handy Haversacks ()"
        printfn "Using path: %s" path
        0 // return an integer exit code
    | _ -> 0