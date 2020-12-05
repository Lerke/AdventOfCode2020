open System
open System.IO

// Transform each line of input into a Seating
type BoardingPass =
    { inputLine: string // Original input line
      row: int option // Final row this passenger gets seated in
      column: int option // Final column this passenger gets seated in
      seatId: int option } // Calculated seat id

// Recursively parse the input line, build a Seating and return when no more input
// is left to be parsed
let rec CalculateRowAndColumn (boardingPass: BoardingPass) (lower, upper) (cLower, cUpper) inputLine =
    match inputLine |> List.length with
    | x when x > 3 ->
        match Seq.head inputLine with
        | 'B' ->
            (CalculateRowAndColumn
                boardingPass
                 (lower
                  + ((((upper - lower) |> double) / 2.0)
                     |> Math.Ceiling
                     |> int),
                  upper)
                 (cLower, cUpper)
                 (List.skip 1 inputLine))
        | 'F' ->
            (CalculateRowAndColumn
                boardingPass
                 (lower,
                  ((lower |> double) + (upper |> double))
                  / 2.0
                  |> int)
                 (cLower, cUpper)
                 (List.skip 1 inputLine))
        | x -> failwithf "Unrecognized input character: %c" x
    | x when x <= 3 && x > 0 ->
        match Seq.head inputLine with
        | 'L' ->
            (CalculateRowAndColumn
                boardingPass
                 (lower, upper)
                 (cLower,
                  ((cLower |> double) + (cUpper |> double))
                  / 2.0
                  |> int)
                 (List.skip 1 inputLine))
        | 'R' ->
            (CalculateRowAndColumn
                boardingPass
                 (lower, upper)
                 (cLower
                  + ((((cUpper - cLower) |> double) / 2.0)
                     |> Math.Ceiling
                     |> int),
                  cUpper)
                 (List.skip 1 inputLine))
        | x -> failwithf "Unrecognized input character: %c" x
    | _ ->
        match (lower, upper), (cLower, cUpper) with
        // Assert we got our calculation right, and calculate the seat Id
        | (x, y), (cX, cY) when x = y && cX = cY ->
            { boardingPass with
                  row = Some x
                  column = Some cX
                  seatId = Some((x * 8) + cX) }
        | _ ->
            failwithf
                "Final boarding pass wasn't calculated correctly. Lower: %d, Upper: %d - Column Lower: %d, Column Upper: %d"
                (lower)
                (upper)
                (cLower)
                (cUpper)

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 1 ->
        let path = Array.head argv
        printfn "Day 5 - Binary Boarding (*)"
        printfn "Using path: %s" path

        // Read input lines as text file, then transform to boarding pass records
        let inputSeats =
            File.ReadAllLines path
            |> Array.toList
            |> List.map (fun x ->
                { inputLine = x
                  row = None
                  column = None
                  seatId = None })
            |> List.map (fun x -> CalculateRowAndColumn x (0, 127) (0, 7) (x.inputLine |> Seq.toList))

        inputSeats
        |> List.iter (fun x -> printfn "Boarding Pass: \"%s\": row %d, column %d, seat ID %d" (x.inputLine) (x.row.Value) (x.column.Value) (x.seatId.Value))

        // Actually find one star output
        let passWithHighestSeatId =
            inputSeats |> List.maxBy (fun g -> g.seatId.Value)

        // Print one star output
        printfn "Boarding Pass with highest Seat ID (One *): \"%s\" - %d" passWithHighestSeatId.inputLine passWithHighestSeatId.seatId.Value

        // Strip first and last rows from our list of boarding passes. Simplifies logic a bit
        let passesWithoutFirstOrLastRows =
            inputSeats
            |> List.filter (fun x -> x.row.Value > 0 && x.row.Value < 127)
            |> List.sortBy (fun x -> x.seatId)

        let ourSeat =
            passesWithoutFirstOrLastRows
            |> List.mapi (fun i s ->
                (s,
                 (if (i + 1 < List.length passesWithoutFirstOrLastRows) then
                     s.seatId.Value
                     - (passesWithoutFirstOrLastRows.[i + 1].seatId.Value)
                     |> Math.Abs
                  else
                      0))) // We'll check where there's a gap of more than 1 seat
            |> List.filter (fun (s, i) -> i = 2) // Actually find our seat

        assert (ourSeat |> List.length = 1) // Should only have one result.

        // We have the seat next to this guy - So just add 1
        printfn "Our seat found (Two *) Seat ID: %d" ((fst (List.head ourSeat)).seatId.Value + 1)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
