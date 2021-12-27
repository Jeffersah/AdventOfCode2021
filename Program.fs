

let inputFile = "./inputs21-25/d25.txt"

let lines = System.IO.File.ReadAllLines(inputFile) |> List.ofArray

printfn "%A" (AdventOfCode2021.Day25.run lines)