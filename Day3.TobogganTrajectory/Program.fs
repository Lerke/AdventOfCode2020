open System
open System.IO
open System.Text.RegularExpressions

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

// Check if the current position is a tree
let SimulationOnTree simulation =
    simulation.world.objects.[(simulation.position.y * simulation.world.width)
                              + simulation.position.x] = Tree

// Run simulation until its conclusion. Essentially, every step take the current world
// check if we can move, check if we're on a tree, actually move. recursively solve until
// we run out of vertical world.
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

// Program input supports pairs of input directions,
// e.g. 3 1 2 4 -> (3,1) and (2,4)
//      1 2     -> (1,2) only
let ParseInputDirections arg =
    Regex.Matches(arg, "(?:(\d+) (\d+))+")
    |> Seq.toList
    |> List.map (fun x ->
        match x.Groups with
        | g when g.Count = 3 ->
            { x = g.[1].Value |> int
              y = g.[2].Value |> int }
        | _ -> failwith (sprintf "Invalid input group. Expected \"x y\" list, got: %s" arg))

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | x when x >= 3 ->
        // First argument is path to input file
        let path = Array.head argv

        // Read our input directions. Each pair will get its own
        // simulation
        let inputDirections =
            (Array.skip 1 argv)
            |> String.concat " "
            |> ParseInputDirections

        printfn "Advent of Code 2020 - Day 3 - Toboggan Trajectory"
        let lines = File.ReadAllLines path |> Array.toList
        let world = ParseInput lines

        // Create an initial simulation with the world we just made
        let initialSimulationStates =
            inputDirections // for each position we'll create an initial simulation
            |> List.map (fun direction ->
                { position = { x = 0; y = 0 }
                  direction =
                      { x = (direction.x |> int)
                        y = (direction.y |> int) }
                  treesHit = 0
                  world = world })

        // Calculate the final state by running initial state to its conclusion
        let finalStates =
            initialSimulationStates |> List.map RunSimulation

        // Do some pretty output printing
        finalStates
        |> List.iteri (fun i world -> printfn "Run %d (%d, %d) - Trees hit: %d" i world.direction.x world.direction.y world.treesHit)

        // Finally just multiply all trees encountered on each slope
        let finalResult =
            finalStates
            |> List.fold (fun c x -> (c |> int64) * (x.treesHit |> int64)) (1 |> int64) // Do some int64 casting to prevent overflowing

        printfn "Multiplied together, that's %d trees! (Two star output)" finalResult
        0 // return an integer exit code
    | _ ->
        printfn "Usage: ./TobogganTrajectory.dll path/to/input.txt initialX initialY [secondInitialX secondInitialY]*"
        1
