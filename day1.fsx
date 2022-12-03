#load "util.fsx"
open Util

let calories =
    inputText.Value
    |> split "\n\n"
    |> Seq.map (split "\n" >> Seq.map int >> Seq.sum)

let part1 = Seq.max calories
let part2 = calories |> Seq.sortDescending |> Seq.take 3 |> Seq.sum

printfn "Part 1: %i\nPart 2: %i" part1 part2