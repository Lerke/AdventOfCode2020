open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open FSharp.Collections.ParallelSeq

type Busline = {
    Interval: uint64
}
type BusInformation = {
    EarliestTime: uint64
    Buslines: (uint64 * Busline) list
}

let ParseInput path =
    match File.ReadAllLines path with
    | [| earliestTimestamp ; departures |] -> {
        EarliestTime = (earliestTimestamp |> uint64)
        Buslines = (departures.Split(',') |> Array.map(fun f -> match f with
                                                                | "x" -> None
                                                                | x -> Some (x |> uint64))
                                                                                   |> Array.mapi (fun i f -> (i, f))
                                                                                   |> Array.filter (fun (i, f) -> f.IsSome)
                                                                                   |> Array.map (fun (i, f) -> ((i |> uint64), {
                                                                                       Interval = f.Value
                                                                                   }))) |> Array.toList }
    | x -> failwithf "Invalid input %A" x

let inline NextBusAt busline (currentTime: uint64) =
    (currentTime + (busline.Interval - (currentTime % busline.Interval)))
    
let rec FindEarliestSubsequentDepartures (buslines: (uint64 * Busline) list) (step: uint64) (time: uint64) :uint64=
    match buslines with
    | (os, x) :: xs -> match (time + os) % (x.Interval) with
                       | 0UL -> FindEarliestSubsequentDepartures xs (step * x.Interval) (time)
                       | _ -> FindEarliestSubsequentDepartures ((os, x) :: xs) (step) (time + step)
    | [] -> (time)
    

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

        printfn "Calculating first number where each bus departs at the subsequent corresponding minute as the first one..."
        let r = FindEarliestSubsequentDepartures businfo.Buslines 1UL 0UL
        printfn $"First time is at {r} minutes (**)"
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        0 // return an integer exit code