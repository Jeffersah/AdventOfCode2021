

let inputFile = "./inputs21-25/d21.txt"

let lines = System.IO.File.ReadAllLines(inputFile) |> List.ofArray

printfn "%A" (AdventOfCode2021.Day21.run2 lines)