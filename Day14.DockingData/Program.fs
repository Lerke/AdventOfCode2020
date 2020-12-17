open System
open System.IO
open FSharp.Text.RegexProvider

type BitmaskElement =
    | Ignore
    | Value of int64
    
type Instruction =
    | Mask of BitmaskElement []
    | Write of (int64 * int64)

type Emulator = {
    Memory: int64 []
    Bitmask: BitmaskElement []
}

type InstructionRegex = Regex< @"^(?<mask>mask = (?<bitmask>(X|1|0)*))$|(?<write>mem\[((?<location>\d+)\]) = (?<wval>\d+))$" >
let ParseInstruction line =
    match InstructionRegex().TryTypedMatch line with
    | Some x -> match (x.mask.Success, x.write.Success) with
                | (true, false) -> Mask (x.bitmask.Value |> Seq.map (fun f -> match f with
                                                                     | 'X' -> Ignore
                                                                     | x -> Value (x |> string |> int64)) |> Seq.toArray)
                | (false, true) -> Write (x.location.Value |> int64, x.wval.Value |> int64)
                | _ -> failwithf "What is this line? %s" line
    | None -> failwithf "Could not parse line: %s" line

let ReplaceArrayIdx (idx: int64) value (array: int64 []) =
    array |> Array.mapi (fun i f -> if (i |> int64) = idx then value else f)
let MaskedWrite (value: int64) (location: int64) (emulator: Emulator) =
    let valueBits = Convert.ToString(value, 2).PadLeft(36, '0') |> Seq.toList
    { emulator with Memory = ReplaceArrayIdx
                                 location
                                 (List.zip valueBits (emulator.Bitmask |> Array.toList)
                                |> List.map (fun (v, m) -> match m with
                                                           | Ignore -> v |> string |> int64
                                                           | Value x -> x)
                                |> fun x -> Convert.ToInt64(String.Join("", x), 2))
                                 emulator.Memory }

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
        let maxAddress = program |> List.choose (fun f -> match f with
                                                  | Write x -> Some x
                                                  | _ -> None)
                                 |> List.maxBy (fst) |> fst |> int
        let emulator = {
            Memory = Array.init (maxAddress + 1) (fun f -> 0L)
            Bitmask = [||]
        }
        
        let resultEmulator = RunProgram emulator program
        let totalSumLeftInMemory = resultEmulator.Memory |> Array.sum
        printfn "Total sum left in memory after completion (*): %d" totalSumLeftInMemory
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1