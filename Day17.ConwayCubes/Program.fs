open System.IO

type CubeState =
    | Inactive
    | Active

and PocketDimension = { Cubes: Cube list }

and Cube =
    { State: CubeState
      Position: (int * int * int) }

let CubeAtPosition position dimension =
    match List.tryFind (fun c -> c.Position = position) dimension.Cubes with
    | Some x -> x
    | None ->
        { State = Inactive
          Position = position }

let NeighbourCoordinates position =
    match position with
    | (x, y, z) ->
        [ (x - 1) .. (x + 1) ]
        |> List.allPairs [ (y - 1) .. (y + 1) ]
        |> List.allPairs [ (z - 1) .. (z + 1) ]
        |> List.map (fun (z, (y, x)) -> (x, y, z))
        |> List.except [ (x, y, z) ]

let NeighbourCubes position dimension =
    NeighbourCoordinates position
    |> List.map (fun p -> CubeAtPosition p dimension)
    |> List.filter (fun p -> p.Position <> position)

let CubesWithState targetState cubes =
    cubes
    |> List.filter (fun f -> f.State = targetState)
    |> List.length

let SimulateRound dimension =
    let newCubes =
        (dimension.Cubes
         |> List.collect (fun f -> NeighbourCubes f.Position dimension)
         |> List.distinct)

    { dimension with
          Cubes =
              newCubes
              |> List.map (fun c ->
                  match (c.State,
                         ((NeighbourCubes c.Position dimension)
                          |> CubesWithState Active),
                         ((NeighbourCubes c.Position dimension)
                          |> CubesWithState Inactive)) with
                  | (Active, x, y) when x = 2 || x = 3 -> c
                  | (Active, x, y) -> { c with State = Inactive }
                  | (Inactive, 3, y) -> { c with State = Active }
                  | (Inactive, _, _) -> c) }

let PrintDimension dimension =
    let indices =
        dimension.Cubes
        |> List.map (fun d ->
            match d.Position with
            | (_, _, z) -> z)
        |> List.distinct
        |> List.sortBy id

    [ (indices |> List.min) .. (indices |> List.max) ]
    |> List.map (fun i ->

        let nodes =
            dimension.Cubes
            |> List.filter (fun d ->
                match d.Position with
                | (_, _, z) -> z = i)
            |> List.sortBy (fun d ->
                match d.Position with
                | (x, y, _) -> (y * 100) + x)
            |> List.groupBy (fun d ->
                match d.Position with
                | (_, y, _) -> y)

        nodes)
    |> List.zip [ (indices |> List.min) .. (indices |> List.max) ]
    |> fun x -> List.except (List.filter (fun (z, y) -> List.forall (fun fg -> fg.State = Inactive) (List.collect (fun fg -> (snd fg)) y)) x) x
    |> List.iter (fun (z, y) ->
        printfn "z=%d" z

        List.iter (fun yy ->
            List.iter (fun yyy ->
                match yyy.State with
                | Inactive -> printf "."
                | Active -> printf "#") (snd yy)

            printfn "") y

        printfn "")

    printfn ""

let rec SimulateNTimes N dimensions =
    match N with
    | x when x > 0 ->
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
                  Position = (ix, iy, 0) }
            | '#' ->
                { State = Active
                  Position = (ix, iy, 0) }
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
        let dimensions = SimulateNTimes cycles [ initial ]

        dimensions
        |> List.iteri (fun i x ->
            printfn "After %d cycles:" (i)
            PrintDimension x)

        let numberOfActiveStates =
            dimensions
            |> List.last
            |> fun x -> CubesWithState Active x.Cubes

        printfn "[ * ] Number of cubes left in active state after %d cycles: %d" cycles numberOfActiveStates
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
