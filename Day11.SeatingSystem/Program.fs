open System
open System.IO

type GridElement =
    | Floor
    | EmptySeat
    | OccupiedSeat

type SeatingGrid = {
    Length: int
    Height: int
    Grid: GridElement [][]
}

type Neighbours = {
    North: GridElement option
    South: GridElement option
    West: GridElement option
    East: GridElement option
    SouthWest: GridElement option
    SouthEast: GridElement option
    NorthWest: GridElement option
    NorthEast: GridElement option
}

let GetSeatingInfo (x,y) grid =
    match (Array.tryItem (y) grid.Grid) with
    | Some a -> Array.tryItem (x) a
    | None -> None

let GetNeighbours (x,y) grid =
    {
        North = GetSeatingInfo (x, y-1) grid
        South = GetSeatingInfo (x, y+1) grid
        West = GetSeatingInfo (x - 1, y) grid
        East = GetSeatingInfo (x + 1, y) grid
        SouthEast = GetSeatingInfo (x + 1, y + 1) grid
        SouthWest = GetSeatingInfo (x - 1, y + 1) grid
        NorthEast = GetSeatingInfo (x + 1, y - 1) grid
        NorthWest = GetSeatingInfo (x - 1, y - 1) grid
    }

let NeighbourCount neighbours =
    [ neighbours.North ; neighbours.South ; neighbours.West ; neighbours.East ; neighbours.SouthEast; neighbours.SouthWest; neighbours.NorthEast ; neighbours.NorthWest ]
    |> List.choose id // Discard none values
    |> List.length

let ParseInputLine line =
    line |> Seq.map (fun f -> match f with
                              | '.' -> Floor
                              | 'L' -> EmptySeat
                              | '#' -> OccupiedSeat
                              | x -> failwithf "Unrecognized input %c in line '%s'" x line)
    |> Seq.toArray

let ParseInput path =
    File.ReadAllLines path
    |> fun x -> { Length = (Array.head x |> Seq.length)
                  Height = (Array.length x)
                  Grid = (x |> Array.map ParseInputLine) }

let CalculateGridPermutation grid =
    let positions = [| 0 .. (grid.Length - 1)|]
                    |> Array.allPairs [| 0 .. (grid.Height - 1) |] |> Array.toSeq |> Seq.sortBy (fun t -> (snd t), (fst t)) |> Seq.toArray
    { grid with Grid = (positions
                        |> Array.map (fun (x,y) -> match (GetSeatingInfo (x,y) grid) with
                                                             | Some z ->
                                                                 match z with
                                                                 | EmptySeat -> (x, y, z)
                                                                 | OccupiedSeat -> (x, y, z)
                                                                 | Floor -> (x, y, z)
                                                             | None -> failwithf "Whoops, invalid index: (%d, %d)!"  x y )
                        |> Array.sortBy (fun (x,y, _) -> x, y)
                        |> Array.groupBy (fun (_, y, _) -> y)
                        |> Array.collect (fun (y, arr) -> arr |> Array.map (fun (p,q,r) -> [|r|]))) }

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        let input = ParseInput path
        let nbTest = GetNeighbours (0,0) input
        printfn "%A" input
        let a = CalculateGridPermutation input
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1