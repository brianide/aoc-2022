#!/usr/bin/env -S dotnet fsi

open System.IO

let addLineAbove (target: string) newLine (lines: string[]) =
    let insertPoint = (lines |> Array.findIndex (fun line -> line.Contains target)) - 1
    Array.concat [lines[.. insertPoint]; [|newLine|]; lines[insertPoint + 1 ..]]

let copyTemplate day =
    File.ReadAllLines "DayTemplate.fs.temp"
    |> Array.updateAt 0 $"module Day{day}"
    |> fun n -> File.WriteAllLines ($"Day{day}.fs", n)

let addToProj day =
    File.ReadAllLines "advent2022.fsproj"
    |> addLineAbove "^ new days go here ^" (sprintf "    <Compile Include=\"Day%s.fs\" />" day)
    |> fun n -> File.WriteAllLines ("advent2022.fsproj", n)

let addToProg day =
    File.ReadAllLines "Program.fs"
    |> addLineAbove "^ new days go here ^" (sprintf "        | \"%s\" -> Day%s.Solvers" day day)
    |> fun n -> File.WriteAllLines ("Program.fs", n)

let () =
    let day = fsi.CommandLineArgs[1]
    copyTemplate day
    addToProj day
    addToProg day