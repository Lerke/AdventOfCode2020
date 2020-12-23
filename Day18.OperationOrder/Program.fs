open System
open System.Collections.Generic
open System.IO

type Element =
    | Number of int64
    | Operator of Operator

and Operator =
    | AssociativeOperator of AssociativeOperator
    | ParamOpen
    | ParamClose

and AssociativeOperator =
    | Add of OperatorAssociativity
    | Subtract of OperatorAssociativity
    | Multiply of OperatorAssociativity
    | Divide of OperatorAssociativity

and OperatorAssociativity = (int * Associativity)

and Associativity =
    | Left
    | Right

and Parser =
    { Operators: Stack<Operator>
      Output: Stack<Element>
      Result: Stack<int64> }

and ProgramMode =
    | PartOne
    | PartTwo

// Tokenize each input line.
let ParseInput (line: string) mode =
    line.Split(" ")
    |> Array.collect (fun c -> c.ToCharArray())
    |> Array.map (fun c ->
        match mode with
        | PartOne ->
            match c with
            | '+' -> Operator(AssociativeOperator(Add(OperatorAssociativity(1, Left))))
            | '-' -> Operator(AssociativeOperator(Subtract(0, Left)))
            | '*' -> Operator(AssociativeOperator(Multiply(1, Left)))
            | '/' -> Operator(AssociativeOperator(Divide(1, Left)))
            | '(' -> Operator ParamOpen
            | ')' -> Operator ParamClose
            | x -> Number(x |> string |> int64)
        | PartTwo ->
            match c with
            | '+' -> Operator(AssociativeOperator(Add(OperatorAssociativity(1, Left))))
            | '-' -> Operator(AssociativeOperator(Subtract(1, Left)))
            | '*' -> Operator(AssociativeOperator(Multiply(0, Left)))
            | '/' -> Operator(AssociativeOperator(Divide(0, Left)))
            | '(' -> Operator ParamOpen
            | ')' -> Operator ParamClose
            | x -> Number(x |> string |> int64))
    |> Array.toList

let rec SolveFormula formula parser =
    // Apply the Shunting Yard formula from Wikipedia to create a
    // reverse polish notation for this formula.
    match formula with
    | x :: xs ->
        match x with
        | Number _ ->
            parser.Output.Push(x)
            SolveFormula xs parser
        | Operator o ->
            match o with
            | ParamOpen ->
                parser.Operators.Push(o)
                SolveFormula xs parser
            | ParamClose ->
                while (match parser.Operators.TryPeek() with
                       | (true, x) when x <> ParamOpen -> true
                       | _ -> false) do
                    parser.Output.Push(Operator(parser.Operators.Pop()))

                if parser.Operators.TryPeek() = (true, ParamOpen)
                then parser.Operators.Pop() |> ignore

                SolveFormula xs parser
            | x ->
                match x with
                | AssociativeOperator op ->
                    while ((match parser.Operators.TryPeek() with
                            | (true, ParamOpen) -> false
                            | _ -> true)
                           && (match parser.Operators.TryPeek() with
                               | (true, (AssociativeOperator operator)) ->
                                   match operator with
                                   | xy ->
                                       match xy with
                                       | Add (x, y)
                                       | Subtract (x, y)
                                       | Multiply (x, y)
                                       | Divide (x, y) ->
                                           match op with
                                           | Add (ox, oy)
                                           | Subtract (ox, oy)
                                           | Multiply (ox, oy)
                                           | Divide (ox, oy) -> if (x > ox || (x = ox && y = oy)) then true else false
                               | _ -> false)) do
                        parser.Output.Push(Operator(parser.Operators.Pop()))

                    parser.Operators.Push x
                    SolveFormula xs parser
                | x -> failwithf "Unexpected token %A. Expected operator instead at this point" x
    | _ ->
        while (match parser.Operators.TryPeek() with
               | (true, _) -> true
               | _ -> false) do
            parser.Output.Push(Operator(parser.Operators.Pop()))

        // So Output now has RPN from right to left. So lets reverse it with
        // strange stack constructor
        let solvedParser =
            { parser with
                  Output = Stack<Element>(parser.Output) }

        while (match solvedParser.Output.TryPeek() with
               | (true, x) -> true
               | _ -> false) do
            match (solvedParser.Output.Pop()) with
            | (Number n) -> solvedParser.Result.Push(n)
            | (Operator (AssociativeOperator op)) ->
                match op with
                | Add x ->
                    solvedParser.Result.Push
                        (solvedParser.Result.Pop()
                         + solvedParser.Result.Pop())
                | Subtract x ->
                    solvedParser.Result.Push
                        (solvedParser.Result.Pop()
                         - solvedParser.Result.Pop())
                | Multiply x ->
                    solvedParser.Result.Push
                        (solvedParser.Result.Pop()
                         * solvedParser.Result.Pop())
                | Divide x ->
                    solvedParser.Result.Push
                        (solvedParser.Result.Pop()
                         / solvedParser.Result.Pop())
            | x -> failwithf "Invalid symbol in RPN calculation: %A" x

        (parser, solvedParser)

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 18 - Operation Order"
        printfn "Path: %s" path

        let formulas =
            [| PartOne; PartTwo |]
            |> Array.map (fun p ->
                File.ReadAllLines path
                |> Array.map (fun f -> ParseInput f p)
                |> Array.map (fun f ->
                    SolveFormula
                        f
                        { Operators = Stack<Operator>()
                          Output = Stack<Element>()
                          Result = Stack<int64>() }) |> Array.toList)
            |> Array.toList


        [| PartOne; PartTwo |]
        |> Array.iter (fun m ->
            printfn "[ %s ] Sum of results: %d"
                (match m with
                 | PartOne -> "*"
                 | PartTwo -> "**")
                ((List.sumBy (fun f -> (snd f).Result.Pop() |> int64)
                      (match m with
                       | PartOne -> List.head formulas
                       | PartTwo -> List.last formulas)) ))
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
