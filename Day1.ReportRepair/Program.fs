open System
open System.IO

let readInput path =
    let numbers = File.ReadLines path |> Seq.map (fun x -> Int32.Parse x) |> Seq.toList
    List.allPairs numbers numbers
    |> List.filter (fun (x,y) -> x <> y) // Exclude doubles
    |> List.filter (fun (x,y) -> x + y = 2020) // Find where sums is 2020
    |> Seq.map (fun (x,y) -> x * y)
    |> Seq.distinct |> Seq.head

[<EntryPoint>]
let main argv =
    let path = Array.head argv
    printfn "Day1.ReportRepair - Using input: %s" path
    let output = readInput path
    printfn "Our output: %d" output;
    0 // return an integer exit code
