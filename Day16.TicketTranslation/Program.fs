open System.IO
open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

type Parser =
    { OurTicket: Ticket
      OtherTickets: Ticket list
      Rules: TicketRules list }

and Ticket = { Values: int list }

and TicketRules =
    { Name: string
      Ranges: (int * int) list }

type RulesRegex = Regex<"(?<classes>.*)\nyour ticket:">
type SingleRuleRegex = Regex<"(?<rule_name>[^\:]+): (?<rl_l>\d+)-(?<rl_h>\d+) or (?<rh_l>\d+)-(?<rh_h>\d+)">
type OurTicketRegex = Regex<"your ticket:\r?\n(?<ticket>.*)nearby tickets:">
type NearbyTicketsRegex = Regex<"nearby tickets:\r?\n(?<other_tickets>.*)">

let ValidInvalidRules (ticket: Ticket) (rules: TicketRules list) =
    ticket.Values
    |> List.fold (fun (cxo, cxn) x ->
        if
            (rules
             |> List.exists(fun z ->
                 (z.Ranges
                  |> List.exists (fun f -> (fst f) <= x && x <= (snd f)))))
        then
            (x :: cxo, cxn)
        else
            (cxo, x :: cxn)) ([], [])

let ParseInput path =
    let input = (File.ReadAllText path)

    match ((RulesRegex(RegexOptions.Multiline ||| RegexOptions.Singleline).TryTypedMatch input),
           (OurTicketRegex(RegexOptions.Multiline ||| RegexOptions.Singleline).TryTypedMatch input),
           (NearbyTicketsRegex(RegexOptions.Multiline ||| RegexOptions.Singleline).TryTypedMatch input)) with
    | (Some rules, Some ourTicket, Some nearbyTickets) ->
        { OurTicket = { Values = ourTicket.ticket.Value
                                     .Replace("\r", "")
                                     .Replace("\n", "")
                                     .Split(",") |> Array.map (int) |> Array.toList }
          OtherTickets = (nearbyTickets.other_tickets.Value
                              .Replace("\r", "")
                              .Split("\n")
                              |> Array.map(fun t -> { Values = t.Split(",") |> Array.map (int) |> Array.toList })
                              |> Array.toList)
          Rules =
              (rules.classes.Value.Replace("\r", "").Split("\n")
               |> Array.toList
               |> List.choose (fun f -> SingleRuleRegex().TryTypedMatch f)
               |> List.filter (fun f -> f.Length > 0)
               |> List.map (fun f ->
                   { Name = f.rule_name.Value
                     Ranges = [ (f.rl_l.Value |> int, f.rl_h.Value |> int) ; (f.rh_l.Value |> int, f.rh_h.Value |> int) ] })) }
    | _ -> failwithf "Unable to parse input: %s" path

let FindRulesForColumn (column: int list) (rules: TicketRules list) =
    rules
    |> List.filter (fun f -> (column |> List.forall (fun c -> (f.Ranges |> List.exists (fun r -> (fst r) <= c && c <= (snd r))))))

let rec Solve (columns: (int list) list) (rules: TicketRules list) (confirmed: (int list * TicketRules list) list) =
    let r = columns
            |> List.map(fun c -> c, FindRulesForColumn c rules)
    let confirmedRules = (r |> List.filter (fun f -> (snd f).Length = 1))
    let confirmedColumns = (List.map (fun f -> (fst f)) confirmedRules)
    match confirmedRules.Length = rules.Length with
    | true -> (List.append confirmedRules confirmed)
    | false -> Solve (List.except confirmedColumns columns) (List.except (List.collect (fun f -> snd f) confirmedRules) rules) (List.append confirmedRules confirmed)

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 16 - Ticket Translation"
        printfn "Path: %s" path

        let input = ParseInput path
        let allTickets = input.OurTicket :: input.OtherTickets
        let ticketErrors = allTickets
                           |> List.map (fun f -> (f, ValidInvalidRules f input.Rules))
        let ticketsWithErrors = ticketErrors |> List.filter (fun f -> (snd (snd f)).Length > 0)
        let ticketScanningErrorRate = ticketsWithErrors |> List.sumBy (fun f -> List.sum (snd (snd f)))

        printfn "[ * ] Ticket Scanning Error Rate: %d" ticketScanningErrorRate

        let ticketsWithoutErrors = ticketErrors |> List.filter (fun f -> (snd (snd f)).Length = 0)
        let columns = ([0 .. (input.OurTicket.Values.Length - 1)]
                       |> List.map (fun f -> List.map (fun x -> (fst x).Values.[f]) ticketsWithoutErrors))

        let solvedColumns = Solve columns input.Rules []
        let departureColumns = solvedColumns
                               |> List.filter (fun f -> ((snd f)
                                                         |> List.head).Name.StartsWith("departure"))
        let departureFieldsMultipliedFromTickets = List.map (fun (i, f) -> i |> List.head) departureColumns
                                                   |> (List.fold (fun c x -> (c |> uint64) * (x |> uint64)) 1UL)
        printfn "[ ** ] Departure columns values multiplied: %d" departureFieldsMultipliedFromTickets
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
