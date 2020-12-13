open System
open System.IO
open System.Text.RegularExpressions

type Height = { unit: string; height: int }
let inline (>=<) a (b, c) = a >= b && a <= c

type Passport =
    { birthYear: int option
      issueYear: int option
      expirationYear: int option
      height: Height option
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
             @"(?:ecl:(?<ecl>(?:amb|blu|brn|gry|grn|hzl|oth)))|(?:pid:(?<pid>\d+))|(?:eyr:(?<eyr>\d{4}))|(?:hcl:(?<hcl>#[a-f0-9]{6}))|(?:byr:(?<byr>\d{4}))|(?:iyr:(?<iyr>\d{4}))|(?:cid:(?<cid>\d+))|(?:hgt:(?<hgt>\d+)(?<hgtUnit>cm|in))")
        |> Seq.toList
        |> List.filter (fun g -> g.Success)

    // Return resulting password, apply validation from two star challenge
    { birthYear =
          (match ExtractFromPassport matches "byr" with
           | Some x when (x |> int) >=< (1920, 2002) -> Some(x |> int)
           | _ -> None)
      issueYear =
          (match ExtractFromPassport matches "iyr" with
           | Some x when (x |> int) >=< (2010, 2020) -> Some(x |> int)
           | _ -> None)
      expirationYear =
          (match ExtractFromPassport matches "eyr" with
           | Some x when (x |> int) >=< (2020, 2030) -> Some(x |> int)
           | _ -> None)
      height =
          (match (ExtractFromPassport matches "hgt", ExtractFromPassport matches "hgtUnit") with
           | (Some h, Some u) when (u = "cm" && ((h |> int) >=< (150, 193))) -> Some { unit = u; height = h |> int }
           | (Some h, Some u) when (u = "in" && ((h |> int) >=< (59, 76))) -> Some { unit = u; height = h |> int }
           | _ -> None)
      hairColour = ExtractFromPassport matches "hcl" // validated by regex
      eyeColour = ExtractFromPassport matches "ecl" // validated by regex
      passportId =
          (match ExtractFromPassport matches "pid" with
           | Some x when x.Length = 9 -> Some x
           | _ -> None)
      countryId = ExtractFromPassport matches "cid" }

// Check if a passport is valid. A passport is valid
// when it has all required fields. For now, the countryId
// field may be ignored when missing.
let (|ValidPassport|InvalidPassport|) passport =
    match passport with
    | { birthYear = Some _; issueYear = Some _; expirationYear = Some _; height = Some _; hairColour = Some _; eyeColour = Some _; passportId = Some _;
        countryId = _ } -> ValidPassport passport
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
        printfn "Day 4 - Passport Processing (**)"
        printfn "Using path: %s" path

        let passports =
            File.ReadAllText(path)
            |> ParseInput
            |> Array.toList
            |> List.map (fun x -> ParsePassportText x)
            |> List.map (fun y -> (y, (IsValidPassport y)))

        let okPasswords =
            (passports |> List.filter (fun (_, s) -> s = true))

        let nokPasswords =
            (passports |> List.filter (fun (_, s) -> s = false))

        printfn @"Number of passports: %d" (List.length passports)
        printfn @"Number of OK passports: %d" (List.length okPasswords)
        printfn @"Number of NOK passports: %d" (List.length nokPasswords)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1
