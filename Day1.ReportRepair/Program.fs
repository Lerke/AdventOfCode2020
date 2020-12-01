open System
open System.IO

let readNumbers (path: string): int list =
    File.ReadLines path |> Seq.map (fun x -> Int32.Parse x) |> Seq.toList

let readInput (numbers: int list): int =
    List.allPairs numbers numbers
    |> List.filter (fun (x,y) -> x <> y) // Exclude doubles
    |> List.filter (fun (x,y) -> x + y = 2020) // Find where sums is 2020
    |> Seq.map (fun (x,y) -> x * y)
    |> Seq.distinct |> Seq.head

let readInputThreePairs (numbers: int list): int =
    List.allPairs (List.allPairs numbers numbers) numbers
    |> List.map (fun ((x,y), z) -> (x,y,z))
    |> List.filter (fun (x,y,z) -> x <> y && x <> z && y <> z)
    |> List.filter (fun (x,y,z) -> x + y + z = 2020)
    |> Seq.map (fun (x,y,z) -> x * y * z)
    |> Seq.distinct |> Seq.head

[<EntryPoint>]
let main argv =
    let path = Array.head argv
    printfn "Day1.ReportRepair - Using input: %s" path
    let numbers = readNumbers path
    
    let output = readInput numbers
    printfn "Our one star output: %i" output
    
    let twoStarOutput = readInputThreePairs numbers
    printfn "Out two star output: %i" twoStarOutput
    0 // return an integer exit code
