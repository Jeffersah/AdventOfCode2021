

let inputFile = "./inputs21-25/d22.txt"

let lines = System.IO.File.ReadAllLines(inputFile) |> List.ofArray

printfn "%A" (AdventOfCode2021.Day22.run2 lines)