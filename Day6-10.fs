namespace AdventOfCode2021
open Utils
module Day6 =
    let RESET_DELAY = 6
    let CREATE_DELAY = 8

    let stepGroup timer (count:uint64) =
        match timer with
        | 0 -> [(RESET_DELAY, count); (CREATE_DELAY, count)]
        | _ -> [(timer - 1, count)]

    let sumGroups (groups:(int*uint64) list) =
        groups |> List.groupBy Tuples.first |> List.map (fun (timer,groups) -> (timer, List.sumBy Tuples.second groups))

    let stepGroups groups =
        groups |> List.collect ((<||) stepGroup) |> sumGroups

    let countAll (groups:(int*uint64) list) = 
        groups |> List.sumBy Tuples.second

    let run (lines:string list) =
        let values = String.split "," lines.[0] |> List.map int
        let groups = values |> List.groupBy id |> List.map (Tuples.mapSecond List.length) |> List.map (Tuples.mapSecond uint64)
        [0..80-1] |> Seq.fold (fun s i -> stepGroups s) groups |> countAll
    
    let runPart2 (lines:string list) =
        let values = String.split "," lines.[0] |> List.map int
        let groups = values |> List.groupBy id |> List.map (Tuples.mapSecond List.length) |> List.map (Tuples.mapSecond uint64)
        [0..256-1] |> Seq.fold (fun s i -> stepGroups s) groups |> countAll

module Day7 =
    let findTarget (positions: int list) =
        positions |> List.map float |> List.average |> round |> int

    let calculateCost positions target =
        List.sumBy (((-) target) >> abs) positions

    let run (lines:string list) =
        let values = String.split "," lines.[0] |> List.map int
        let minX,maxX = List.min values, List.max values
        [minX..maxX] |> List.map (calculateCost values) |> List.min

    let factorial n = Seq.init (n+1) id |> Seq.sum
    let factorial_cached = cache factorial
    
    let calculateCost2 positions target =
        List.sumBy (((-) target) >> abs >> factorial_cached) positions
    
    let run2 (lines:string list) =
        let values = String.split "," lines.[0] |> List.map int
        let minX,maxX = List.min values, List.max values
        [minX..maxX] |> List.map (calculateCost2 values) |> List.min

module Day8 =
    let displayPerDigit = [|"abcefg"; "cf"; "acdeg"; "acdfg"; "bcdf"; "abdfg"; "abdefg"; "acf"; "abcdefg"; "abcdfg"|]

    let run lines =
        let outputOnly = 
            lines |> 
            List.map (String.split " | ") |> 
            List.map (fun l -> l.[1]) |>
            List.collect (String.split " ")
        outputOnly |> List.where (fun i ->
            match String.length i with
            | 2 | 3 | 4 | 7 -> true
            | _ -> false ) |> List.length

    type TestCase = string list * string list

    let parseTestCase line =
        let [left; right] = String.split " | " line |> List.map (String.split " ")
        left,right

    let firstFromSet set = set |> Seq.head

    let applyMap (map:Map<char,char>) str =
        String.map (fun ch -> map.[ch]) str

    let reverseMap map =
        Map.toSeq map |> Seq.map Tuples.flip |> Map.ofSeq

    let lookupDigit str =
        let str = str |> Seq.sort |> Seq.map string |> String.concat ""
        Array.findIndex ((=)str) displayPerDigit

    let runCase fullSeq output =
        let charSets = fullSeq |> List.groupBy (String.length) |> List.map (Tuples.mapSecond (fun strs -> strs |> List.map Set.ofSeq |> List.reduce Set.intersect)) |> Map.ofSeq
        let outputMap = Map.empty
        let outputMap = Map.add 'a' (Set.difference charSets.[3] charSets.[2] |> firstFromSet) outputMap // a = 3 - 2
        let outputMap = Map.add 'd' (Set.intersect charSets.[4] charSets.[5] |> firstFromSet) outputMap // d = 4 & 5
        let outputMap = Map.add 'c' (Set.difference charSets.[3] charSets.[6] |> firstFromSet) outputMap // c = 3 - 6
        let outputMap = Map.add 'f' (Set.difference charSets.[2] (Set.ofList [outputMap.['c']]) |> firstFromSet) outputMap // f = 2 - [c]
        let outputMap = Map.add 'g' (Set.difference charSets.[5] (Set.ofList [outputMap.['a'];outputMap.['d']]) |> firstFromSet) outputMap // g = 5 - ad
        let outputMap = Map.add 'b' (Set.difference charSets.[4] (Set.ofList [outputMap.['c'];outputMap.['d'];outputMap.['f']]) |> firstFromSet) outputMap // b = 4 - cdf
        let usedValues = outputMap |> Map.toSeq |> Seq.map Tuples.second |> Set.ofSeq
        let eValue = ['a'..'g'] |> List.find (fun key -> Set.contains key usedValues |> not)
        let finalMap = reverseMap(Map.add 'e' eValue outputMap)
        let outputChars = output |> List.map (applyMap finalMap)
        let [a;b;c;d] = outputChars |> List.map lookupDigit
        d+(10*c)+(100*b)+(1000*a)

    let run2 lines =
        let testCases = lines |> List.map parseTestCase
        testCases |> List.map ((<||) runCase) |> List.sum

module Day9 =

    let parseInput lines =
        let rows = List.map (fun line -> line |> Seq.map string |> Seq.map int |> List.ofSeq) lines
        let xLength = rows.[0].Length
        let yLength = rows.Length
        Array2D.init xLength yLength (fun x y -> rows.[y].[x])

    let isLowPoint map x y =
        let height = Array2D.get map x y
        let neighbors = Array2D.neighbors map x y
        Seq.forall ((<)height) neighbors

    let getRiskLevel map x y =
        let height = Array2D.get map x y
        if isLowPoint map x y then 1 + height else 0

    let run lines =
        let map = parseInput lines
        Array2D.foldi (fun c x y _ -> c + getRiskLevel map x y) 0 map

    let rec expandAndReplace map x y =
        let neighborPos = Array2D.orthoNeighborIndecies map x y
        let validNeighbors = neighborPos |> Seq.where (fun (x,y) ->
            let value = Array2D.get map x y
            value <> 0 && value <> 9)
        Array2D.set map x y 0
        validNeighbors |> Seq.fold (fun ct (x,y) -> ct + expandAndReplace map x y) 1

    let getAndReplaceBasin map x y =
        if isLowPoint map x y |> not then 0
        else expandAndReplace map x y

    let run2 lines =
        let map = parseInput lines
        let basins = Array2D.flatMapi (fun x y v -> getAndReplaceBasin map x y) map |> Seq.sortDescending |> List.ofSeq
        basins.[..2] |> List.reduce (*) 

module Day10 =
    let openChars = Set.ofList ['(';'{';'[';'<']
    let closeChars = Set.ofList [')';'}';']';'>']
    let openClose = Map.ofList [('(',')');('[',']');('{','}');('<','>')]
    let scores = Map.ofList [(')',3);(']',57);('}',1197);('>',25137)]
    let scores2 = Map.ofList [(')',1UL);(']',2UL);('}',3UL);('>',4UL)]

    let rec removeValidPairs input =
        match input with
        | [] -> [], false
        | [v] -> [v], false
        | a::r when Set.contains a closeChars -> 
            let result, removed = removeValidPairs r
            a::result, removed
        | a::(b::r) ->
            if openClose.[a] = b then 
                let result, _ = removeValidPairs r
                result, true
            else 
                let result, removed = removeValidPairs (b::r)
                a::result, removed

    let rec repeatedlyRemoveValidPairs input =
        match removeValidPairs input with
        | (output, false) -> output
        | (output, true) -> repeatedlyRemoveValidPairs output

    let getScore input =
        let result = repeatedlyRemoveValidPairs input
        let firstClosingBracket = result |> List.tryFind (closeChars.Contains)
        Option.map (fun b -> scores.[b]) firstClosingBracket |> Option.defaultValue 0
        
    let run lines =
        let inputs = lines |> List.map List.ofSeq
        inputs |> List.sumBy getScore

    let getCompletionString input =
        let result = repeatedlyRemoveValidPairs input
        if List.exists (fun e -> Set.contains e closeChars) result then None
        else result |> List.rev |> List.map (fun e -> openClose.[e]) |> Some

    let getScore2 completionString =
        List.fold (fun score next -> score * 5UL + scores2.[next]) 0UL completionString

    let run2 lines =
        let inputs = lines |> List.map List.ofSeq
        let scores = inputs |> List.choose getCompletionString |> List.map getScore2 |> List.sort
        scores.[scores.Length / 2]
