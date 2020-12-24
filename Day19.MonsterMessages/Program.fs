open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

type TerminalPattern = Regex< @"(?<ruleId>\d+): \""?(?<ruleChar>([a-z]+\|?)+)\""?">
type NonTerminalPattern = Regex<"(?<ruleId>\d+): (?<rules>\d.*)">
type InputPattern = Regex<"^(?<rule>[ab]+)">
type RulePattern = Regex<"^(?<ruleId>\d+): (?<pattern>.*)">
type Rule = {
    Index: int
    Pattern: string
}

let rec ParseRules lines =
    let (terminals, nonTerminals) =
        ((TerminalPattern().TypedMatches lines), (NonTerminalPattern().TypedMatches lines))

    let nonterminalsToReplace =
        nonTerminals
        |> Seq.choose (fun f ->
            match (f.rules.Value.Split(" ")
                   |> Seq.forall (fun f ->
                       match f with
                       | "|" -> true
                       | x when (terminals
                                 |> Seq.exists (fun g -> g.ruleId.Value = x)) -> true
                       | _ -> false)) with
            | true -> Some f
            | false -> None)
    let newTerminals =
        nonterminalsToReplace
        |> Seq.map (fun f -> sprintf "%s: %s"
                                 f.ruleId.Value
                                 (f.rules.Value.Replace(" ", "")
                                  |> Seq.map (fun r ->  match r with
                                                        | '"' -> ""
                                                        | '|' -> (r |> string)
                                                        | _ -> (terminals
                                                               |> Seq.find (fun nt -> nt.ruleId.Value = (r |> string))).ruleChar.Value)
                                                               |> Seq.concat |> Seq.toArray |> String))
        |> Seq.append (terminals |> Seq.map (fun x -> x.Value))
        
    let newNonterminals =
        nonTerminals
        |> Seq.except nonterminalsToReplace
        |> Seq.map (fun f -> f.Value)
        
    let newResults =  (Seq.append newNonterminals newTerminals) 
    match Seq.isEmpty newNonterminals with
    | false -> ParseRules (String.Join("\n", newResults))
    | true -> newResults
              |> Seq.map (fun f -> f.Replace("\"", ""))
              |> Seq.map (fun f -> match RulePattern().TryTypedMatch f with
                                   | Some s -> { Pattern = s.pattern.Value
                                                 Index = s.ruleId.Value |> int }
                                   | None -> failwithf "Could not parse input: %s" f)

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        let rules = ParseRules(File.ReadAllText path)
        let inputLines = InputPattern(RegexOptions.Multiline).TypedMatches (File.ReadAllText path) |> Seq.map (fun f -> f.rule.Value)
        let RuleWithIdxZero = rules |> Seq.find (fun f -> f.Index = 0)
        
        let linesMatchingRuleZero =
            inputLines |> Seq.map (fun f -> Regex.IsMatch(f, $"^({RuleWithIdxZero.Pattern})$"), f)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
