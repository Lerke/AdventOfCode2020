﻿open System.IO
open System.Text.RegularExpressions

let InitialBagPropertyPattern = @"^(\w+\s\w+) bags contain"
let AdditionalBagPropertyPattern = @"(\d+) (\w+\s\w+) bags?"

type BagChildConstraint = {
    bagType: string
    number: int
}

type Bag = {
    bagType: string // e.g. 'Shiny Gold', 'Faded Blue', etc.
    children: BagChildConstraint list
}

type BagTreeNode = {
    label: string
    leaves: BagTreeNode list
}

let rec ColourIsReachableFromNode (bag: BagTreeNode) (colour: string): bool =
    match bag.label = colour with
    | true -> true
    | false when bag.leaves.Length > 0 -> (List.fold (fun c g -> c || (ColourIsReachableFromNode g colour)) false bag.leaves)
    | _ -> false

let rec ExpandNode (bag: Bag) (bagMap: Map<string, Bag>): BagTreeNode =
    { label = bag.bagType
      leaves = bag.children
                   |> List.map (fun x -> ExpandNode (Map.find x.bagType bagMap) bagMap) }

let ParseInput (path: string) =
    let lookup = File.ReadAllLines path
                                |> Array.map (fun x -> {
                                bagType = (Regex.Match(x, InitialBagPropertyPattern).Groups
                                           |> Seq.skip 1
                                           |> Seq.toList
                                           |> List.map (fun f -> f.Value)
                                           |> String.concat " ")
                                children = Regex.Matches(x, AdditionalBagPropertyPattern)
                                           |> Seq.map (fun y -> {
                                               bagType = y.Groups.[2].Value
                                               number = y.Groups.[1].Value |> int
                                               }) |> Seq.toList })
                                |> Array.map (fun f -> (f.bagType, f))
                                |> Map.ofArray
    lookup
    |> Map.toList
    |> List.map snd
    |> List.map (fun x -> (ExpandNode x lookup))

[<EntryPoint>]
let main argv =
    match argv with
    | [| path; colour|] ->
        printfn "Day 7 - Handy Haversacks ()"
        printfn "Using path: %s" path
        printfn "Using colour: %s" colour

        let parsedInput = ParseInput path

        // To calculate how many initial bag colours can eventually contain at least one shiny gold bag, we'll take all our
        // root bags and check if any children have the label 'shiny gold'
        let reachableFrom =
            parsedInput
            |> List.filter (fun x -> x.label <> colour) // we don't care about this trivial case.
            |> List.map (fun x -> (x.label, ColourIsReachableFromNode x colour))
            |> List.filter (fun (f,s) -> s)
        printfn "The %s bag is reachable from %d initial bags!" colour reachableFrom.Length
        0 // return an integer exit code
    | _ -> 1