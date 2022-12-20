module Day19

open System.Collections.Generic
open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

//Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.

type Resource =
    Ore | Clay | Obsidian | Geode
    static member Each = [Ore; Clay; Obsidian; Geode]

type Inventory = Map<Resource, int>
type CostSpec = Resource * int
type Blueprint = {Id: int; Costs: Map<Resource, CostSpec list>}

type Strategy = Build of Resource | Wait

// Options:
// 1. Do nothing (save); only valid if at least one robot is both UNBUILDABLE and WILL BECOME SO
// 2. Build an available robot type (each type must be considered)
//    Probably safe to assume building geode robot will be optimal if we're able to do it

// Filter builds that would result in higher production of a resource than the maximum cost (excluding itself) involving it

let inline (?>) opt def = Option.defaultValue def opt
let inline trinum n = max 0 (n * (n + 1) / 2)

let parse file =
    let parseLine =
        let reg = "Blueprint (\\d+): \
                   Each ore robot costs (\\d+) ore. \
                   Each clay robot costs (\\d+) ore. \
                   Each obsidian robot costs (\\d+) ore and (\\d+) clay. \
                   Each geode robot costs (\\d+) ore and (\\d+) obsidian."
        String.regGroups reg
        >> List.map int
        >> function
        | [id; ore; clay; obsidOre; obsidClay; geodeOre; geodeOsid] ->
            let bill = [
                Ore, [Ore, ore]
                Clay, [Ore, clay]
                Obsidian, [Ore, obsidOre; Clay, obsidClay]
                Geode, [Ore, geodeOre; Obsidian, geodeOsid]
            ]
            {Id = id; Costs = Map.ofList bill}
        | _ -> failwith "Invalid input"
    File.ReadAllLines file
    |> Array.map parseLine

let searchPaths blueprint maxTime =
    let mutable maxGeodes = 0

    let canBuild (inv: Inventory) kind =
        blueprint.Costs[kind]
        |> Seq.forall (fun (res, amt) -> inv[res] >= amt)

    let tickBots inv bots = 
        Map.toSeq bots
        |> Seq.fold (fun acc (k, n) -> Map.add k (acc[k] + n) acc) inv

    let buildRobot inv (bots: Inventory) kind =
        let inv = blueprint.Costs[kind] |> Seq.fold (fun acc (k, n) -> Map.add k (acc[k] - n) acc) inv
        let bots = Map.add kind (bots[kind] + 1) bots
        (inv, bots)

    let getStrats (inv: Inventory) (bots: Inventory) time buildable =
        let timeLeft = maxTime - time
        let potGeodes = inv[Geode] + bots[Geode] * timeLeft + trinum (timeLeft - 1)

        if timeLeft <= 0 then
            []
        elif potGeodes < maxGeodes then
            []
        elif Seq.contains Geode buildable then
            [Build Geode]
        else
            let notMaxed kind = Map.values blueprint.Costs |> Seq.collect id |> Seq.filter (fst >> (=) kind) |> Seq.map snd |> Seq.max > bots[kind]
            Seq.filter notMaxed buildable
            |> Seq.map Build
            |> Seq.toList
            |> List.append [Wait]

    let rec recur =
        let func (inv: Inventory) bots log time =
            if inv[Geode] > maxGeodes then
                maxGeodes <- inv[Geode]
                printfn "%A" maxGeodes

            if time = maxTime then
                [inv, List.rev log]
            else
                let buildable = Resource.Each |> List.filter (canBuild inv)

                getStrats inv bots time buildable
                |> Seq.map (fun strat ->
                    let inv = tickBots inv bots
                    // let log = (time + 1, strat) :: log
                    match strat with
                    | Build kind ->
                        let (inv, bots) = buildRobot inv bots kind
                        recur inv bots log (time + 1)
                    | Wait ->
                        recur inv bots log (time + 1))
                // |> Seq.maxBy (fun (inv, _) -> inv[Geode])
                |> Seq.collect id
                |> Seq.toList

        let dict = Dictionary<_, _>()
        fun inv bots log time ->
            let key = (inv, bots, time)
            match dict.TryGetValue key with
            | true, value ->
                value
            | _ -> 
                let value = func inv bots log time
                dict.Add(key, value)
                value

    let inv = Resource.Each |> List.map (Tuple2.withSnd 0) |> Map.ofList
    let bots = Resource.Each |> List.map (Tuple2.withSnd 0) |> Map.ofList |> Map.add Ore 1
    recur inv bots [] 0

let solveSilver (input: Blueprint[]) =
    searchPaths input[0] 24
    |> Seq.map fst
    |> Seq.maxBy (fun k -> k[Geode])
    |> printfn "%A" 
    ""

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold