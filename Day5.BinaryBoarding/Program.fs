open System
open System.IO

// Transform each line of input into a Seating
type Seating =
    { inputLine: string // Original input line
      row: int option // Final row this passenger gets seated in
      column: int option // Final column this passenger gets seated in 
      seatId: int option } // Calculated seat id

// Recursively parse the input line, build a Seating and return when no more input
// is left to be parsed
let rec CalculateRowAndColumn (seating: Seating) (lower, upper) (cLower, cUpper) inputLine =
    match inputLine |> List.length with
    | x when x > 3 -> // Calculate first seven characters F & B
        match Seq.head inputLine with
        | 'B' ->
            (CalculateRowAndColumn
                seating
                 (lower
                  + ((((upper - lower) |> double) / 2.0)
                     |> Math.Ceiling
                     |> int),
                  upper)
                 (cLower, cUpper)
                 (List.skip 1 inputLine))
        | 'F' ->
            (CalculateRowAndColumn
                seating
                 (lower,
                  ((lower |> double) + (upper |> double))
                  / 2.0
                  |> int)
                 (cLower, cUpper)
                 (List.skip 1 inputLine))
        | x -> failwithf "Unrecognized input character: %c" x
    | x when x <= 3 && x > 0 -> // Match last three characters L & R
        match Seq.head inputLine with
        | 'L' ->
            (CalculateRowAndColumn seating (lower, upper) (cLower,
                  ((cLower |> double) + (cUpper |> double))
                  / 2.0
                  |> int) (List.skip 1 inputLine))
        | 'R' -> 
            (CalculateRowAndColumn seating (lower, upper) (cLower
                  + ((((cUpper - cLower) |> double) / 2.0)
                     |> Math.Ceiling
                     |> int),
                  cUpper) (List.skip 1 inputLine))
        | x -> failwithf "Unrecognized input character: %c" x
    | _ ->
        match (lower, upper), (cLower, cUpper) with
        // Assert we got our calculation right, and calculate the seat Id
        | (x, y), (cX, cY) when x = y && cX = cY -> { seating with row = Some x; column = Some cX; seatId = Some ((x * 8) + cX) } 
        | _ -> failwithf "Final seat wasn't calculated correctly. Lower: %d, Upper: %d - Column Lower: %d, Column Upper: %d" (lower) (upper) (cLower) (cUpper)

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 1 ->
        let path = Array.head argv
        printfn "Day 5 - Binary Boarding (*)"
        printfn "Using path: %s" path
        let inputLines =
            File.ReadAllLines path
            |> Array.toList
            |> List.map (fun x ->
                { inputLine = x
                  row = None
                  column = None
                  seatId = None })
            |> List.map (fun x -> CalculateRowAndColumn x (0, 127) (0, 7) (x.inputLine |> Seq.toList))
            
        inputLines
        |> List.iter (fun x -> printfn "Boarding Pass: \"%s\": row %d, column %d, seat ID %d" (x.inputLine) (x.row.Value) (x.column.Value) (x.seatId.Value))
        
        let passWithHighestSeatId = inputLines |> List.maxBy (fun g -> g.seatId.Value)
        printfn "Boarding Pass with highest seat ID: \"%s\" - %d" passWithHighestSeatId.inputLine passWithHighestSeatId.seatId.Value
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
