open System.IO
open System.Text.RegularExpressions

let inline (>=<) a (b,c) = a >= b && a <= c
type PasswordInput = {
    lowerBound: int;
    upperBound: int
    character: char
    password: string;
}

let checkValidity input =
    input
    |> List.map(fun x -> (x, ((List.fold (fun c y -> if y = x.character then c + 1 else c) 0 (Seq.toList x.password)) >=< (x.lowerBound, x.upperBound))))

let ParseToInputStruct input =
    let m = Regex.Match(input, "^(\d+)-(\d+) (\w{1}): (.*)$")
    if m.Success then Some {
        lowerBound = m.Groups.[1].Value |> int
        upperBound = m.Groups.[2].Value |> int
        character = m.Groups.[3].Value |> char
        password = m.Groups.[4].Value
    } else None

// Define a function to construct a message to print
let readInput path =
    File.ReadAllLines path |> Array.toList
    |> List.map (fun x -> ParseToInputStruct x)
    |> List.filter (fun x -> x.IsSome)
    |> List.map (fun x -> x.Value)
    |> checkValidity
    |> List.filter (fun (_, valid) -> valid)

[<EntryPoint>]
let main argv =
    let path = Array.head argv
    let output = readInput path

    // Print output
    output |> List.iter (fun (x,y) -> (printfn "Pattern: %d-%d/%c Password: %s - Valid: %b" x.lowerBound x.upperBound x.character x.password y))
    printfn "Number of valid passwords (Part 1): %d" (List.length output)
    0 // return an integer exit code