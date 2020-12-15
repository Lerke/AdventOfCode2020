open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open FSharp.Collections.ParallelSeq

type Busline = {
    Interval: bigint
}
type BusInformation = {
    EarliestTime: bigint
    Buslines: (bigint * Busline) list
}

let ParseInput path =
    match File.ReadAllLines path with
    | [| earliestTimestamp ; departures |] -> {
        EarliestTime = (BigInteger.Parse(earliestTimestamp))
        Buslines = (departures.Split(',') |> Array.map(fun f -> match f with
                                                                | "x" -> None
                                                                | x -> Some (BigInteger.Parse(x)))
                                                                                   |> Array.mapi (fun i f -> (i, f))
                                                                                   |> Array.filter (fun (i, f) -> f.IsSome)
                                                                                   |> Array.map (fun (i, f) -> ((i |> bigint), {
                                                                                       Interval = f.Value
                                                                                   }))) |> Array.toList }
    | x -> failwithf "Invalid input %A" x

let NextBusAt busline (currentTime: bigint) =
    (currentTime + (busline.Interval - (currentTime % busline.Interval)))

let SubsequentBusDepartures (buslines: (bigint * Busline) list) offset =
    PSeq.forall (fun f -> NextBusAt (snd f) (offset - 1I) % (offset + (fst f)) = 0I) buslines

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 13 - Shuttle Search"
        printfn "Path: %s" path
        let businfo = ParseInput path
        // Find next departure times of each bus
        let earliestBus = businfo.Buslines
                          |> List.map (fun f -> (f, (NextBusAt (snd f) businfo.EarliestTime)))
                          |> List.sortBy (fun (x, y) -> y)
                          |> List.head
        printfn $"""Earliest bus that will depart is at T={snd earliestBus}, Bus={(snd (fst earliestBus)).Interval}.
You will have to wait {snd earliestBus} - {businfo.EarliestTime} = {snd earliestBus - businfo.EarliestTime} minutes
Multiplied by Id (*) -> {snd earliestBus - businfo.EarliestTime} * {(snd (fst earliestBus)).Interval} = {(snd earliestBus - businfo.EarliestTime) * ((snd (fst earliestBus)).Interval)} minutes"""

        let sw = Stopwatch()
        sw.Start()
        printfn "Calculating first number where each bus departs at the subsequent corresponding minute as the first one..."
        
        let firstBus = businfo.Buslines |> List.head |> snd
        let result = Seq.unfold (fun n -> Some(n, n + firstBus.Interval)) firstBus.Interval
                     |> PSeq.tryFind (fun f -> let result = SubsequentBusDepartures businfo.Buslines f
                                               printfn "%A: %b" f result
                                               result)
        sw.Stop()
        printfn $"Finally done! Only took {sw.ElapsedMilliseconds / 1000L} seconds!"
        printfn $"First time is at {result.Value} minutes (**)"
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        0 // return an integer exit code