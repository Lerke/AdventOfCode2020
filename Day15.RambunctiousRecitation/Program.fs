open System.Collections.Generic
open System.IO

let ParseInput path =
    (File.ReadAllText path).Split(",")
    |> Seq.map (int)
    |> Seq.toList |> List.rev
    
let RoundCache = Dictionary<int, int list> []  
let rec CalculateRoundsNumberSpoken spokenNumbers round target =
    match round with
    | x when x = target -> spokenNumbers
    | _ ->
        let lastNumberSpoken = spokenNumbers |> List.head
        match RoundCache.TryGetValue lastNumberSpoken with
        | (true, lastRound) when (List.length lastRound) > 1 ->
            let lastTurnSpoken = (lastRound |> List.head)
            let turnBeforeThen = (lastRound |> List.skip 1 |> List.head)
            let newNumber = ((lastTurnSpoken - turnBeforeThen))
            match RoundCache.TryGetValue newNumber with
            | (true, existing) ->
                RoundCache.[newNumber] <- [ round ; (existing |> List.head ) ]
            | _ ->
                RoundCache.Add(newNumber, [ round ])
            CalculateRoundsNumberSpoken (newNumber :: spokenNumbers) (round + 1) target
        | (_, _) ->
            // Last number had not yet been spoken. Next number is zero
            match RoundCache.TryGetValue 0 with
            | (true, zero) ->
                RoundCache.[0] <- [ round ; (zero |> List.head) ]
            | _ ->
                RoundCache.Add(0, [ round ])
            CalculateRoundsNumberSpoken ( 0 :: spokenNumbers ) (round + 1) target

[<EntryPoint>]
let main argv =
    match argv with
    | [| path; targetRound |] ->
        printfn "Day 15 - Rambunctious Recitation"
        printfn "Path: %s" path
        printfn "N: %d" (targetRound |> int)
        let input = ParseInput path
        printfn "Input: %A" input
        input |> List.rev |> List.iteri (fun i f -> RoundCache.TryAdd(f, [i]) |> ignore)
        printfn "The number spoken at round %d is (*): %d" (2020) (CalculateRoundsNumberSpoken input (input |> List.length) (2020) |> List.head)
        printfn "The number spoken at round %d is (**): %d" (targetRound |> int) (CalculateRoundsNumberSpoken input (input |> List.length) (targetRound |> int) |> List.head)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt 2020"
        1
        