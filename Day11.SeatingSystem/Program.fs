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
    |> fun x -> (List.length x, (x |> List.filter (fun n -> n = EmptySeat) |> List.length), (x |> List.filter (fun n -> n = OccupiedSeat) |> List.length))

//let LineOfSightNeighbours neighbours =
//    0
//    
let ApplyOneStarRules gridElement neighbours =
    match ( gridElement, (NeighbourCount neighbours)) with
    | (EmptySeat, (_, _, occupied)) when occupied = 0 -> OccupiedSeat // No occupied seats adjacent to this seat
    | (OccupiedSeat, (_, _, occupied)) when occupied >= 4 -> EmptySeat // Occupied seats become empty when 4 or more adjacent occupied seats
    | (seat, _) -> seat // Else nothing

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
    
let PrintGrid grid =
    printfn "" |> ignore
    grid.Grid
    |> Array.iter (fun x -> (Seq.iter (fun c -> printf "%c" (match c with | EmptySeat -> 'L' | OccupiedSeat -> '#' | Floor -> '.' | _ -> '?')) x)
                            (printf "\n") )

let CalculateGridPermutation grid =
    { grid with Grid = [| 0 .. (grid.Height - 1)|]
                      |> Array.map (fun y ->
                                            Array.map (fun x -> match GetSeatingInfo (x,y) grid with
                                                                | Some z -> (ApplyOneStarRules z (GetNeighbours (x,y) grid))
                                                                | None -> failwithf "Whoops, invalid index (%d, %d)" x y) [| 0 ..  (grid.Length - 1) |]) }

// Simulate n number of rounds on the grid
let rec PermuteGrid grid count =
    let permutation = CalculateGridPermutation grid
    match count with
    | x when x > 0 -> permutation :: PermuteGrid permutation (count - 1)
    | _ -> [ grid ]

let GridDiff A B =
    (A.Grid |> Array.collect id) |> Array.zip (B.Grid |> Array.collect id)
    |> Array.filter (fun (a, b) -> a <> b)
    |> Array.length
    
let rec PermuteUntilStable grid permutations =
    let permutation = CalculateGridPermutation grid
    match GridDiff grid permutation with
    | x when x > 0 -> ((permutation, permutations) :: PermuteUntilStable permutation (permutations + 1)) // Change was detected. Keep running
    | _ -> [ (grid, permutations) ] // No more change detected. Return last grid and count

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        let input = ParseInput path
        let stableGrid = (PermuteUntilStable input 0) |> List.last
        let occupiedSeats = (((fst stableGrid).Grid |> Array.collect id) |>
                             Array.filter (fun x -> x = OccupiedSeat)) |> Array.length
        printf "Last stable grid"
        PrintGrid (fst stableGrid)
        printfn "Number of occupied seats (*): %d" occupiedSeats
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1