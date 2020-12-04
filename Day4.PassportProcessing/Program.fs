open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Passport =
    { birthYear: string option
      issueYear: string option
      expirationYear: string option
      height: string option
      hairColour: string option
      eyeColour: string option
      passportId: string option
      countryId: string option }

// Retrieve a key from a match list result of some passport text
let ExtractFromPassport passportMatches key =
    let groups =
        passportMatches
        |> List.collect (fun (m: Match) ->
            (m.Groups
             |> Seq.toList
             |> List.filter (fun j -> j.Success && j.Name <> "0")))

    match List.tryFindIndex (fun (y: Group) -> (y.Name) = key) groups with
    | Some x -> Some groups.[x].Value
    | None -> None

// take a single block of input and extract a Passport
// out of it
let ParsePassportText (text: String) =
    let matches =
        Regex.Matches
            (text,
             @"(?:ecl:(?<ecl>#?\w+))|(?:pid:(?<pid>[^\s]+))|(?:eyr:(?<eyr>\d+))|(?:hcl:(?<hcl>#?\w+))|(?:byr:(?<byr>\d+))|(?:iyr:(?<iyr>\d+))|(?:cid:(?<cid>\d+))|(?:hgt:(?<hgt>\d+\w+))")
        |> Seq.toList
        |> List.filter (fun g -> g.Success)

    { birthYear = ExtractFromPassport matches "byr"
      issueYear = ExtractFromPassport matches "iyr"
      expirationYear = ExtractFromPassport matches "eyr"
      height = ExtractFromPassport matches "hgt"
      hairColour = ExtractFromPassport matches "hcl"
      eyeColour = ExtractFromPassport matches "ecl"
      passportId = ExtractFromPassport matches "pid"
      countryId = ExtractFromPassport matches "cid" }

// Check if a passport is valid. A passport is valid
// when it has all required fields. For now, the countryId
// field may be ignored when missing.
let (|ValidPassport|InvalidPassport|) passport =
    match passport with
    | { birthYear = Some _; issueYear = Some _; expirationYear = Some _; height = Some _; hairColour = Some _; eyeColour = Some _; passportId = Some _; countryId = _ } ->  // ignore CountryId for now (Part 1)
        ValidPassport passport
    | _ -> InvalidPassport passport

let IsValidPassport passport =
    match passport with
    | ValidPassport _ -> true
    | InvalidPassport _ -> false

// Take input text and transform in into list of passports.
let ParseInput inputLines =
    Regex.Split(inputLines, @"^\s*$", RegexOptions.Multiline ||| RegexOptions.Singleline)
    |> Array.filter (fun y -> y.Trim().Length > 0)

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        let path = Array.head argv
        printfn "Day 4 - Passport Processing (*)"
        printfn "Using path: %s" path
            
        let passports =
            File.ReadAllText(path)
            |> ParseInput
            |> Array.toList
            |> List.map (fun x -> ParsePassportText x)
            |> List.map (fun y -> (y, (IsValidPassport y)))
        let okPasswords = (passports |> List.filter (fun (_, s) -> s = true))
        let nokPasswords = (passports |> List.filter (fun (_, s) -> s = false))

        printfn @"Number of passports: %d" (List.length passports)
        printfn @"Number of OK passports: %d" (List.length okPasswords)
        printfn @"Number of NOK passports: %d" (List.length nokPasswords)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
