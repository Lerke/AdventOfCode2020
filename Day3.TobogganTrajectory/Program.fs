open System
open System.IO

// The world exists of only two objects
type WorldObject =
    | Empty
    | Tree

type Position = { x: int; y: int }

// Solution simulates a 'world', which is just a typed mapping
// of the input
type World =
    { width: int // Width of each input line
      length: int // Total number of input lines
      objects: WorldObject list } // Parsed lines

// The program simulates a toboggan racing down
// a slope with an initial position and direction
type Simulation =
    { position: Position // Current position
      direction: Position // Initial direction, e.g. right 3 down 1
      treesHit: int
      world: World }

let SimulationOnTree simulation =
    simulation.world.objects.[(simulation.position.y * simulation.world.width)
                              + simulation.position.x] = Tree

// Run simulation until its conclusion
let rec RunSimulation simulation =
    match simulation with
    // When the next step won't result in us falling off the end of the slope, return a new world state
    | _ when (simulation.position.y + simulation.direction.y) < simulation.world.length ->
        RunSimulation  // Recursively calculate until we're done
            { simulation with
                  position =
                      { x = (simulation.position.x + simulation.direction.x) % simulation.world.width
                        y = simulation.position.y + simulation.direction.y }
                  treesHit =
                      match simulation |> SimulationOnTree with
                      | true -> simulation.treesHit + 1
                      | false -> simulation.treesHit }
    | _ ->
        { simulation with
              treesHit =
                  match simulation |> SimulationOnTree with
                  | true -> simulation.treesHit + 1
                  | false -> simulation.treesHit }

// Transform one line of input to a list of world objects
let ParseInputLine (line: String): WorldObject list =
    line
    |> Seq.toList // To chars
    |> List.map (fun x ->
        match x with
        | '#' -> Tree
        | '.' -> Empty
        | a -> failwith (sprintf "Unrecognized input: %c, in line: %s" a line))

// Parse an entire list of world data into a world
let ParseInput lines =
    { width = (lines |> List.head |> String.length) // World width is just number of chars per line
      length = (List.length lines) // World length is just total number of lines in input
      objects =
          lines
          |> List.map (fun x -> ParseInputLine x) // Parse every line for a list of lists of world objects
          |> List.collect (fun x -> x) } // Flatten this list list to a single list.

[<EntryPoint>]
let main argv =
    match argv with
    | [| path; initialX; initialY |] ->
        printfn "Advent of Code 2020 - Day 3 - Toboggan Trajectory"
        printfn "Using path: %s\nDirection: (%d, %d)" path (initialX |> int) (initialY |> int)
        let lines = File.ReadAllLines path |> Array.toList
        let world = ParseInput lines

        // Create an initial simulation with the world we just made
        let initialSimulationState =
            { position = { x = 0; y = 0 }
              direction =
                  { x = (initialX |> int)
                    y = (initialY |> int) }
              treesHit = 0
              world = world }

        // Calculate the final state by running initial state to its conclusion
        let finalState = RunSimulation initialSimulationState

        printfn "Okay, done sledding.\nTrees hit: %d" finalState.treesHit
        0 // return an integer exit code
    | _ ->
        printfn "Usage: ./TobogganTrajectory.dll path/to/input.txt initialX initialY"
        1
