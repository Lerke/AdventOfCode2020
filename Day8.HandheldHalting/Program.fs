open System.IO
open FSharp.Text.RegexProvider

// Supported operations by the handheld
type Operation =
    | Accumulate of int
    | Jump of int
    | Nop of int

// Input text is parsed to operations using a regular expression
type PatternRegex = Regex< @"^(?<operation>\w{3})\s(?<argument>(?:\+|-)\d+)$" >

// Each memory location holds an operation type, the original input and a list of cycles
// at which this location was visited.
type MemoryLocation = {
    visitedAt: int list
    input: string
    operation: Operation
}

// We're going to emulate a small CPU with a memory map, IP and Accumulator.
type HandheldConsole = {
    InstructionPointer: int
    MemoryMap: MemoryLocation []
    Accumulator: int
    CycleCount: int
}

// Pretty print the state of the console
let PrintHandheldState console =
    printfn
        "Handheld Console ---\nInstruction Pointer: %d\nAccumulator: %d\nInstructions: %d\nCycles Simulated: %d"
        console.InstructionPointer
        console.Accumulator
        (console.MemoryMap |> Array.length)
        console.CycleCount

// Update the amount of times a certain memory position was visited by appending the
// current IP location.
let UpdateMemoryVisits memoryMap index cpuCycle =
    Array.mapi (fun i x -> if i = index then { x with visitedAt = (cpuCycle) :: x.visitedAt } else x) memoryMap

// Recursively parse our program / memory map until we run out of memory to execute.
let rec RunProgram console cpuCycle =
    // Retrieve current op
    match ((console.InstructionPointer)) < (Array.length console.MemoryMap) with
    // Check if we have visited before. If not: execute this instruction!
    | true when (console.MemoryMap.[console.InstructionPointer].visitedAt |> List.length) = 0 ->
            match ((console.MemoryMap.[console.InstructionPointer])).operation with
            // For ACC, increase the accumulator by the argument, then increase the IP by 1
            | Accumulate x -> RunProgram ({ console with
                                                MemoryMap = (UpdateMemoryVisits console.MemoryMap console.InstructionPointer cpuCycle)
                                                InstructionPointer = console.InstructionPointer + 1
                                                Accumulator = console.Accumulator + x
                                                CycleCount = cpuCycle}) (cpuCycle + 1)
            // For JMPs, just add x to the instruction pointer and keep going.
            | Jump x -> RunProgram ({ console with
                                        MemoryMap = (UpdateMemoryVisits console.MemoryMap console.InstructionPointer cpuCycle)
                                        InstructionPointer = console.InstructionPointer + x
                                        CycleCount = cpuCycle }) (cpuCycle + 1)
            // For NOPs, do nothing but increase the IP by 1
            | Nop x -> RunProgram ({ console with
                                        MemoryMap = (UpdateMemoryVisits console.MemoryMap console.InstructionPointer cpuCycle)
                                        InstructionPointer = console.InstructionPointer + 1
                                        CycleCount = cpuCycle}) (cpuCycle + 1)
    // We have an instruction, but have visited this memory location before. Halt.
    | true ->
        printfn
            "Halting on instruction %s @ Instruction Pointer %d.\nReason: Already visited this instruction before on cycle: %A"
            console.MemoryMap.[console.InstructionPointer].input
            console.InstructionPointer
            console.MemoryMap.[console.InstructionPointer].visitedAt
        console
    | false ->
        // We ran out of program. Halt
        printfn "Halting! Reached end of memory map"
        { console with CycleCount = cpuCycle }

// Turn a line of input into a typed operation
let ParseOperation input =
    match PatternRegex().TypedMatch(input) with
    | m when m.Success ->
        match (m.operation.Value, m.argument.Value) with
        | ("acc", x) -> { input = input; operation = Accumulate (x |> int); visitedAt = [] }
        | ("jmp", x) -> { input = input ; operation = Jump (x |> int); visitedAt = [] }
        | ("nop", x) -> { input = input ; operation = Nop (x |> int); visitedAt = [] }
        | (x, y)     -> failwithf "Unrecognized program operation %s%s" y x
    | _ ->
        failwithf "Unrecognized input: %s" input

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 8 - Handheld Halting (*)"
        printfn "Using path: %s" path
        let initialConsoleState = {
            InstructionPointer = 0
            MemoryMap = (File.ReadAllLines path)
                        |> (Array.filter (String.length >> (fun x -> x > 0)))
                        |> (Array.map ParseOperation)
            Accumulator = 0 // Accumulator is initialized at 0
            CycleCount = 1 // We have not done any cycles yet.
        }
        let output = RunProgram initialConsoleState 1
        PrintHandheldState output
        printfn "Final value of accumulator before infinite loop (*): %d" output.Accumulator
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
