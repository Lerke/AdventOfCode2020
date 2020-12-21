open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open FSharp.Collections.ParallelSeq

type CubeState =
    | Inactive
    | Active

and PocketDimension = { Cubes: Cube list }

and Cube =
    { State: CubeState
      Position: (int * int * int * int) }

let mutable CubeCache =
    ConcurrentDictionary<(int * int * int * int), Cube>()

let inline CubeAtPosition position dimension =
    match CubeCache.TryGetValue position with
    | (true, x) -> x
    | (false, _) ->
        match List.tryFind (fun c -> c.Position = position) dimension.Cubes with
        | Some x -> x
        | None ->
            let r =
                { State = Inactive
                  Position = position }

            CubeCache.TryAdd(r.Position, r) |> ignore
            r

let inline NeighbourCoordinates position =
    match position with
    | (x, y, z, w) ->
        [ (x - 1) .. (x + 1) ]
        |> List.allPairs [ (y - 1) .. (y + 1) ]
        |> List.allPairs [ (z - 1) .. (z + 1) ]
        |> List.allPairs [ (w - 1) .. (w + 1) ]
        |> List.map (fun (w, (z, (y, x))) -> (x, y, z, w))
        |> List.except [ (x, y, z, w) ]

let inline NeighbourCubes position dimension =
    NeighbourCoordinates position
    |> List.map (fun p -> CubeAtPosition p dimension)
    |> List.filter (fun p -> p.Position <> position)

let inline CubesWithState targetState cubes =
    cubes
    |> List.filter (fun f -> f.State = targetState)
    |> List.length

let SimulateRound dimension =
    let newCubes =
        (dimension.Cubes
         |> PSeq.collect (fun f -> NeighbourCubes f.Position dimension)
         |> PSeq.toList
         |> List.distinct)

    { dimension with
          Cubes =
              newCubes
              |> PSeq.map (fun c ->
                  match (c.State,
                         ((NeighbourCubes c.Position dimension)
                          |> CubesWithState Active),
                         ((NeighbourCubes c.Position dimension)
                          |> CubesWithState Inactive)) with
                  | (Active, x, y) when x = 2 || x = 3 -> c
                  | (Active, x, y) -> { c with State = Inactive }
                  | (Inactive, 3, y) -> { c with State = Active }
                  | (Inactive, _, _) -> c)
              |> PSeq.toList }

let PrintDimension dimension =
    (List.groupBy (fun f ->
        match f.Position with
        | (x, y, z, w) -> w) dimension.Cubes)
    |> List.map (fun f ->
        List.groupBy (fun fz ->
            match fz.Position with
            | (x, y, z, w) -> z) (snd f))
    |> List.map (fun f ->
        List.map (fun (w, fw) ->
            List.groupBy (fun fz ->
                match fz.Position with
                | (x, y, z, w) -> y) fw) f)
    |> (List.iteri (fun iw w ->
            List.iteri (fun iz z ->
                printfn ""

                printfn
                    "z=%d w=%d"
                    (iz
                     - (((w |> List.length |> double) / 2.0)
                        |> floor
                        |> int))
                    (iw
                     - (((w |> List.length |> double) / 2.0)
                        |> floor
                        |> int))

                List.iter (fun y ->
                    List.iter (fun x ->
                        match x.State with
                        | Active -> printf "#"
                        | Inactive -> printf ".") (snd y)

                    printfn "") z) w))

let rec SimulateNTimes N dimensions =
    match N with
    | x when x > 0 ->
        printfn "Running simulation %d" N
        CubeCache.Clear()
        SimulateNTimes
            (N - 1)
            ((SimulateRound(List.head dimensions))
             :: dimensions)
    | 0 -> dimensions |> List.rev

let ParseInput path =
    File.ReadAllLines path
    |> Array.toList
    |> List.mapi (fun iy x ->
        Seq.mapi (fun ix y ->
            match y with
            | '.' ->
                { State = Inactive
                  Position = (ix, iy, 0, 0) }
            | '#' ->
                { State = Active
                  Position = (ix, iy, 0, 0) }
            | _ -> failwithf "Unknown input symbol: %c" y) x
        |> Seq.toList)
    |> List.collect id

[<EntryPoint>]
let main argv =
    match argv with
    | [| path; cycles |] ->
        printfn "Day 17 - Conway Cubes"
        printfn "Path: %s" path

        let cycles = cycles |> int
        printfn "Cycles: %d" cycles

        let initial = { Cubes = ParseInput path }
        let sw = Stopwatch()
        sw.Start()
        let dimensions = SimulateNTimes cycles [ initial ]
        sw.Stop()

        dimensions
        |> List.iteri (fun i x ->
            printfn "After %d cycles:" (i)
            PrintDimension x)

        let numberOfActiveStates =
            dimensions
            |> List.last
            |> fun x -> CubesWithState Active x.Cubes

        printfn "[ ** ] Number of cubes left in active state after %d cycles: %d\nSimulation took %d seconds!" cycles numberOfActiveStates sw.Elapsed.Seconds
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
