open System
open System.IO
open FSharp.Text.RegexProvider

type EmulatorVersion =
    | One
    | Two

type BitmaskElement =
    | Ignore
    | Value of int64
    
type Instruction =
    | Mask of BitmaskElement []
    | Write of (int64 * int64)

type MemoryLocation = {
    Index: int64
    Value: int64
}

type Emulator = {
    Memory: MemoryLocation list
    Bitmask: BitmaskElement []
    Version: EmulatorVersion
}

type InstructionRegex = Regex< @"^(?<mask>mask = (?<bitmask>(X|1|0)*))$|(?<write>mem\[((?<location>\d+)\]) = (?<wval>\d+))$" >
type FloatingRegex = Regex< @"X" >

let ParseInstruction line =
    match InstructionRegex().TryTypedMatch line with
    | Some x -> match (x.mask.Success, x.write.Success) with
                | (true, false) -> Mask (x.bitmask.Value |> Seq.map (fun f -> match f with
                                                                     | 'X' -> Ignore
                                                                     | x -> Value (x |> string |> int64)) |> Seq.toArray)
                | (false, true) -> Write (x.location.Value |> int64, x.wval.Value |> int64)
                | _ -> failwithf "What is this line? %s" line
    | None -> failwithf "Could not parse line: %s" line

let ReplaceArrayIdx (idx: int64) (value: int64) (array: MemoryLocation list) =
    match (List.tryFind (fun f -> f.Index = idx) array) with
    | Some x -> array |> List.map (fun f -> if (f.Index) = idx then { f with Value = value } else f)
    | None -> { Index = idx; Value = value } :: array

let rec ExpandFloatingBits maskStrings =
    maskStrings
    |> List.collect(fun maskString ->
        match Regex.Match(maskString, "X") with
        | m -> match m.Success with
               | true -> ExpandFloatingBits ([ (FloatingRegex().Replace(maskString, "1", 1)) ; FloatingRegex().Replace(maskString, "0", 1) ])
               | false -> maskStrings)
    |> List.distinct
     
let rec RecMaskedWrite (value: int64) (locations: string list) (emulator) =
    match locations with
    | x :: xs -> RecMaskedWrite value xs { emulator with Memory =
                                            (ReplaceArrayIdx
                                                 (Convert.ToInt64(x, 2))
                                                 value
                                            emulator.Memory) }
    | [] -> emulator
    
let ApplyMask (value: int64) (mask: BitmaskElement[]) =
    Array.zip ( Convert.ToString(value, 2).PadLeft(36, '0') |> Seq.toArray) (mask)
        |> Array.map (fun (v, m) -> match m with
                               | Ignore -> "X"
                               | Value x when x = 0L -> v |> string
                               | Value x when x <> 0L -> x |> string)
        |> Array.toList
        |> fun x -> String.Join("", x)
    
let MaskedWrite (value: int64) (location: int64) (emulator: Emulator) =
    let valueBits = Convert.ToString(value, 2).PadLeft(36, '0') |> Seq.toList
    match emulator.Version with
    | One -> { emulator with Memory =
                (ReplaceArrayIdx location
                (List.zip valueBits (emulator.Bitmask |> Array.toList)
                    |> List.map (fun (v, m) -> match m with
                        | Ignore -> v |> string |> int64
                        | Value x -> x)
                    |> fun x -> Convert.ToInt64(String.Join("", x), 2)) emulator.Memory) }
    | Two -> RecMaskedWrite value (ExpandFloatingBits [ (ApplyMask location emulator.Bitmask) ]) emulator

let ReadProgram path =
    File.ReadAllLines path
    |> Array.map (ParseInstruction)
    |> Array.toList
    
let rec RunProgram emulator program =
    match program with
    | x :: xs -> match x with
                 | Mask m -> RunProgram { emulator with Bitmask = m } xs
                 | Write (loc, wval) -> RunProgram (MaskedWrite wval loc emulator) xs
    | [] -> emulator

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 14 - Docking Data"
        printfn "Path: %s" path
        
        let program = ReadProgram path
        let emulator = {
            Memory = []
            Bitmask = [||]
            Version = One
        }
        
        let resultEmulator = RunProgram emulator program
        let totalSumLeftInMemory = resultEmulator.Memory |> List.sumBy (fun f -> f.Value)
        printfn "Total sum left in memory after completion with v1 (*): %d" totalSumLeftInMemory
        
        let emulatorTwo = {
            Memory = []
            Bitmask = [||]
            Version = Two
        }
        
        let resultEmulatorV2 = RunProgram emulatorTwo program
        let totalSumLeftInMemoryV2 = resultEmulatorV2.Memory |> List.sumBy (fun f -> f.Value)
        printfn "Total sum left in memory after completion with v2 (**): %d" totalSumLeftInMemoryV2
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1