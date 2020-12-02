open System.IO
open System.Text.RegularExpressions

// Provide some helpful inline operators for Task 1 / Task 2
let inline (>=<) a (b,c) = a >= b && a <= c
let inline (>><<) (s:string) (u , l , c) = ((s.[u-1] <> s.[l-1]) && ((s.[u-1] = c && s.[l-1] <> c) || (s.[u-1] <> c && s.[l-1] = c)))

// Each line will be transformed into a typed record
type PasswordInput = {
    lowerBound: int;
    upperBound: int
    character: char
    password: string;
}

// Apply rules for Task 1 & Task 2 to list of input records
let checkValidityPart1 input =
    input
    |> List.map(fun x -> (x, ((List.fold (fun c y -> if y = x.character then c + 1 else c) 0 (Seq.toList x.password)) >=< (x.lowerBound, x.upperBound)))) // One star

let checkValidityPart2 input =
    input
    |> List.map(fun (x) -> if x.password >><< (x.lowerBound, x.upperBound, x.character) then (x, true) else (x, false)) // Two star

// Transform input strings into records
let ParseToInputStruct input =
    let m = Regex.Match(input, "^(\d+)-(\d+) (\w{1}): (.*)$")
    if m.Success then Some {
        lowerBound = m.Groups.[1].Value |> int
        upperBound = m.Groups.[2].Value |> int
        character = m.Groups.[3].Value |> char
        password = m.Groups.[4].Value
    } else None

// Read inputs. Parse them to structs, apply rule validation
// Then filter out invalid passwords. Return set of validated passwords.
let readInput path =
    File.ReadAllLines path |> Array.toList
    |> List.map (fun x -> ParseToInputStruct x)
    |> List.filter (fun x -> x.IsSome)
    |> List.map (fun x -> x.Value)

[<EntryPoint>]
let main argv =
    let path = Array.head argv
    let outputOne = readInput path
                    |> checkValidityPart1
                    |> List.filter (fun (_, valid) -> valid)

    let outputTwo = readInput path
                    |> checkValidityPart2
                    |> List.filter (fun (_, valid) -> valid)

    // Print output
    printfn "-- Part 1 -- "
    outputOne |> List.iter (fun (x,y) -> (printfn "Pattern: %d-%d/%c Password: %s - Valid: %b" x.lowerBound x.upperBound x.character x.password y))

    printfn "-- Part 2 -- "
    outputTwo |>List.iter (fun (x,y) -> (printfn "Pattern: %d-%d/%c Password: %s - Valid: %b" x.lowerBound x.upperBound x.character x.password y))

    printfn "Number of valid passwords (Part 1/2): %d / %d" (List.length outputOne) (List.length outputTwo)
    0 // return an integer exit code