open System
open System.IO
open FSharp.Text.RegexProvider

type ShipRegex = Regex < @"(?<direction>N|S|E|W|L|R|F)(?<amount>\d+)" > 
type ShipAction =
    | N of int
    | S of int
    | E of int
    | W of int
    | L of int
    | R of int
    | F of int

type Direction =
    | North
    | East
    | South
    | West

type SteerDirection =
    | Left
    | Right

let Action line =
    match ShipRegex().TryTypedMatch line with
    | Some m when m.Success -> match (m.direction.Value, m.amount.Value |> int) with
                               | ("N", a) -> N a
                               | ("S", a) -> S a
                               | ("E", a) -> E a
                               | ("W", a) -> W a
                               | ("L", a) -> L a
                               | ("R", a) -> R a
                               | ("F", a) -> F a
                               | (x, _) -> failwithf "Unknown input symbol %s" x
    | None -> failwithf "Unsupported input string %s" line

type Ship = {
    Location: (int * int)
    Direction: Direction
}

type ShipWithWaypoint = {
    Location: (int * int)
    Waypoint: (int * int)
}

let rec Turn (initialDirection: Direction) (direction: SteerDirection) (degrees: int) =
    match degrees with
    | x when (x) >= 90 -> match initialDirection with
                          | North -> match direction with
                                     | Left -> (Turn West Left (degrees - 90))
                                     | Right -> (Turn East Right (degrees - 90))
                          | East -> match direction with
                                    | Left -> (Turn North Left (degrees - 90))
                                    | Right -> (Turn South Right (degrees - 90))
                          | South -> match direction with
                                     | Left -> (Turn East Left (degrees - 90))
                                     | Right -> (Turn West Right (degrees - 90))
                          | West -> match direction with
                                    | Left -> (Turn South Left (degrees - 90))
                                    | Right -> (Turn North Right (degrees - 90))
    | _ -> initialDirection
 
let RotateVector (vector: (int * int)) direction =
     match (vector, direction) with
     | ((vx, vy), Left) -> (-vy, vx)
     | ((vx, vy), Right) -> (vy, -vx)
    
let rec TurnWaypoint (waypointPosition: int * int) (direction: SteerDirection) (degrees: int): int * int =
    match (degrees, waypointPosition) with
    | (x, y) when x >= 90 -> TurnWaypoint (RotateVector waypointPosition direction) direction (degrees - 90)
    | _ -> waypointPosition
    
let ApplyAction (ship: Ship) action =
    match action with
    | N _ | S _ | E _ | W _ -> {
        ship with Location = match ship.Location with
                             | (cx, cy) -> match action with
                             | N x -> (cx, cy + x)
                             | S x -> (cx, cy - x)
                             | E x -> (cx + x, cy)
                             | W x -> (cx - x, cy) }
    | L x -> { ship with Direction = (Turn ship.Direction Left x) }
    | R x -> { ship with Direction = (Turn ship.Direction Right x) }
    | F x -> { ship with Location = match ship.Location with
                                    | (cx, cy) -> match ship.Direction with
                                                  | North -> (cx, cy + x)
                                                  | South -> (cx, cy - x)
                                                  | East -> (cx + x, cy)
                                                  | West -> (cx - x, cy) }

let ApplyActionToWaypoint shipWithWaypoint action =
    match action with
    | N _ | S _ | E _ | W _ -> {
        shipWithWaypoint with Waypoint = match shipWithWaypoint.Waypoint with
                                         | (cwx, cwy) -> match action with
                                                         | N x -> (cwx, cwy + x)
                                                         | S x -> (cwx, cwy - x)
                                                         | E x -> (cwx + x, cwy)
                                                         | W x -> (cwx - x, cwy) }
    | L x -> { shipWithWaypoint with Waypoint = (TurnWaypoint shipWithWaypoint.Waypoint Left x)  }
    | R x -> { shipWithWaypoint with Waypoint = (TurnWaypoint shipWithWaypoint.Waypoint Right x)  }
    | F x -> { shipWithWaypoint with Location = match (shipWithWaypoint.Location, shipWithWaypoint.Waypoint) with
                                                | ((cx, cy), (cwx, cwy)) -> ((cx + (x * cwx)), (cy + (x * cwy))) }

let rec ApplyMovesToShip (ship: Ship) (moves: ShipAction list): Ship =
    match moves with
    | x :: xs -> ApplyMovesToShip (ApplyAction ship x) xs
    | [] -> ship
    
let rec ApplyMovesToWaypointShip (ship: ShipWithWaypoint) (moves: ShipAction list): ShipWithWaypoint =
    match moves with
    | x :: xs -> ApplyMovesToWaypointShip (ApplyActionToWaypoint ship x) xs
    | [] -> ship
    
let ManhattanDistance (location: int * int): int =
    match location with 
    | (cx, cy) -> (cx |> abs) + (cy |> abs)

let ParseInput path =
    File.ReadLines path
    |> Seq.toArray
    |> Array.map Action
    |> Array.toList

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        let moves = ParseInput path
        let initial = {
            Direction = East
            Location = (0, 0)
        }

        let shipAfterMoves = ApplyMovesToShip initial moves
        let (shipX, shipY) = shipAfterMoves.Location
        let oneStarOutput = ManhattanDistance shipAfterMoves.Location
        printfn "Manhattan distance between origin (0,0) and current location (%d, %d) (*): %d" shipX shipY oneStarOutput
        
        let initialTwoStar = {
            Location = (0, 0)
            Waypoint = (10, 1)
        }
        
        let shipWaypointAfterMoves = ApplyMovesToWaypointShip initialTwoStar moves
        let (shipWaypointX, shipWaypointY) = shipWaypointAfterMoves.Location
        let twoStarOutput = ManhattanDistance shipWaypointAfterMoves.Location
        printfn "Manhattan distance between origin (0,0) and current location (%d, %d) (**): %d" shipWaypointX shipWaypointY twoStarOutput
        
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1