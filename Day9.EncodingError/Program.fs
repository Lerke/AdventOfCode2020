open System
open System.IO

type XmasInput = {
    Numbers: int64 list
    PreambleLength: int
}

// Recursively process the list of input numbers until we find a number
// that doesn't match the rules:
//    - Number should be the sum of any two of the <preamble> previous numbers
//    - There may be more than one such pair present
//    - Pairs with equal components do not count
let rec ValidateXmasEncoding input iteration =
    let offset = ((iteration |> int64) - (input.PreambleLength |> int64)) |> int
    match (offset >= 0) with
    | true ->
        // Check if current number matches. If not, return the number and index.
        // Get pairs of numbers
        let skippedList = input.Numbers
                          |> List.skip offset
                          |> List.take input.PreambleLength
        match skippedList
              |> List.allPairs(skippedList)
              |> List.filter (fun (x, y) -> x <> y)
              |> List.tryFind (fun (x, y) -> (x + y) = input.Numbers.[iteration]) with
              | Some (x, y) ->
                  // We have a pair that sums to this number. Keep going.
                  ValidateXmasEncoding input (iteration + 1)
              | _ ->
                  // There's no pairs that add to this number. We've found our one star
                  // match
                  (input.Numbers.[iteration], iteration)
    | false ->
        // No check needed. We're still parsing the preamble, just continue
        ValidateXmasEncoding input (iteration + 1)

let CalculateContiguousSets input =
    [ 0 .. (input.Numbers |> List.length) ]
    |> List.map int64
    |> List.map (fun f ->
        let remaining = (List.skip (f |> int) input.Numbers)
        [ 0 .. (remaining |> List.length) - 1 ]
        |> List.map(fun f -> (List.take f remaining)))
    |> List.collect id
    |> List.filter (fun f -> f.Length >= 2) // We're only interested in lists with at least 2 elements
    |> List.distinct

[<EntryPoint>]
let main argv =
    match argv with
    | [| path; preamble |] when fst (Int32.TryParse preamble) = true ->
        printfn "Day 9 - Encoding Error (**)"
        printfn "Path: %s\nPreamble: %s" path preamble

        let initialInput = {
            Numbers = (File.ReadAllLines path |> (Array.map (int64))) |> List.ofArray
            PreambleLength = (preamble |> int)
        }
        let (output, iteration) = ValidateXmasEncoding initialInput 0
        printfn "First number that is not a combination of any two pairs (*): %d at index %d" output iteration

        // Calculate contiguous sets. Then label them with their sums.
        // Then check where their sum equals our forbidden number!
        // Then just give the min, max and totals for that two star output.
        match CalculateContiguousSets initialInput
                             |> List.map (fun f -> (f, List.sum(f)))
                             |> List.filter (fun (x, y) -> y = output)
                             |> List.tryHead with
                             | Some x -> printfn
                                             "Found range that sums to %d!\nRange: %A\nSmallest: %d\nLargest: %d\nTotal: %d (**)"
                                             output
                                             (fst x)
                                             (List.min (fst x))
                                             (List.max (fst x))
                                             ((List.min (fst x)) + (List.max (fst x)))
                                         0
                             | None ->
                                 printf "Could not find a contiguous range that sums to %d!" output
                                 1
    | _ ->
        printfn "Usage: dotnet run ./path/to/input preambleLength"
        1