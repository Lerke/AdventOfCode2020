open System.IO
open System.Text.Json
open FSharp.Collections.ParallelSeq

let inline (>><<) a (b,c) = a >= b && a <= c

type Adapter = {
    Joltage: int
}

type TreeNode = {
    Data: Adapter
    Children: TreeNode list
}

// Return a new list without a certain adapter
let without list el =
    match List.tryFindIndex (fun f -> f.Joltage = el.Joltage) list with
    | Some x ->
        List.mapi(fun i f -> (i, f)) list
        |> (List.fold (fun a f -> if (fst f) = x then a else (snd f :: a)) [])
    | None -> list
    
// Recursively figure out the max depth of this tree
let rec MaxDepth depth (tree:TreeNode) path =
    match (List.isEmpty tree.Children) with
    | true -> (depth + 1, { tree with Children = [] } :: path)
    | false -> List.maxBy (fun f -> fst f) (tree.Children |> List.map(fun f -> MaxDepth (depth + 1) f ({tree with Children = []} :: path)))

// Take a list of adapters, turn it into a tree of all possible adapter
// combinations provided a given initial node
let rec BuildTree (adapters: Adapter list) (initial: TreeNode) =
    match adapters with
    | [] -> initial
    | x :: xs ->
        { initial with Children = (adapters
                                   |> List.filter (fun f -> f.Joltage >><< (initial.Data.Joltage, initial.Data.Joltage + 3))
                                   |> List.min
                                   |> fun f -> [ (BuildTree (without adapters f) { Data = f; Children = [] }) ]
  ) }

// Take a path to the input text file, turn it into a list of adapters
let ParseInput path =
    File.ReadAllLines path
    |> Array.map (fun x ->  { Joltage = (x |> int) })
    |> Array.toList
    |> fun x -> ((List.filter (fun y -> y.Joltage >><< (1,3))) x)
                 |> List.sortBy (fun f -> f.Joltage)
                 |> List.head
                 |> fun f -> BuildTree (without x f) { Data = f; Children = [] }

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "Day 10 - Adapter Array (*)"
        printfn "Path: %s" path
        
        let trees = ParseInput path
        // Calculate max depth by taking all trees from initial adapters, then calculating
        // the max depth for each. Then selecting the max among the results.
        let maxTree = (MaxDepth 0 trees [])
        printfn "The longest adapter chain is %d adapters.\nHighest input voltage: %d\nBuilt in Joltage Adapter: %d"
            (fst maxTree)
            (List.head (snd maxTree)).Data.Joltage
            ((List.head (snd maxTree)).Data.Joltage + 3)
        
        let maxTreeWithBeginAndEnd =
            List.append ({
                Data = { Joltage = ((snd maxTree)
                                     |> (List.maxBy (fun f -> f.Data.Joltage))).Data.Joltage + 3 }
                Children = []
            } :: (snd maxTree)) [ { Data = { Joltage = 0 } ; Children = [] } ]
        let differenceTree =
            maxTreeWithBeginAndEnd
            |> List.take ((List.length (maxTreeWithBeginAndEnd)) - 1)
            |> List.zip (List.skip 1 (maxTreeWithBeginAndEnd))
        let (p,q,r) = differenceTree
                          |> List.fold (fun (p,q,r) (x, y) -> match ((x.Data.Joltage - y.Data.Joltage) |> abs) with
                                                              | 3 -> (p, q, r + 1)
                                                              | 2 -> (p, q + 1, r)
                                                              | 1 -> (p + 1, q , r)
                                                              | x -> failwithf "Found a difference larger than 3! Incompatible chain: %d" x) (0,0,0)
        printfn "--- Joltage Differences (*) ---\n1 Jolt: %d\n2 Jolt: %d\n3 Jolt: %d\nMultiplied (*): %d"
            p
            q
            r
            (p * r)
        0
    | _ ->
        printfn "Usage: dotnet run ./path/to/input.txt"
        1