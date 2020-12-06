open System.IO
open System.Text.RegularExpressions

// Our program parses the input into groups.
type Group = {
    numberOfPeople: int // Number of people in this group
    answers: char [] []// Answers per person in the group. Each index represents the answers of a single person
    uniqueQuestionsYes: int // number of unique questions this group answered yes to.
}

// Takes the full input text, and transforms it into a list of Groups.
let ParseGroups fullInput =
    Regex.Split(fullInput, "^\s*$", RegexOptions.Multiline ||| RegexOptions.Singleline)
    |> Array.map (fun x -> x.Trim('\n')) // Trim pre/pro-ceding newline tokens
    |> Array.map (fun x -> { // Create Groups from input string
        numberOfPeople = (x.Split("\n").Length)
        answers = (x.Split("\n")) |> Array.map(fun y -> (y.ToCharArray() ))
        uniqueQuestionsYes = 0 })// Set at next step
    |> Array.map (fun x -> { // As above, but set unique questions answered here instead of calculating later.
        x with uniqueQuestionsYes = (x.answers
                                     |> Array.collect (fun y -> y)
                                     |> Array.distinct).Length }) // Flatten & count distinct

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        let path = Array.head argv
        printfn "Day 6 - Custom Customs (*)"
        printfn "Using path: %s" path
        let input = (File.ReadAllText path).Replace("\r\n", "\n") // Normalize line endings
        let groups = ParseGroups input

        // One star output: Sum all unique answers
        printfn "Sum of questions to which anyone answered yes: %d" (Array.sumBy (fun f -> f.uniqueQuestionsYes) groups)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1