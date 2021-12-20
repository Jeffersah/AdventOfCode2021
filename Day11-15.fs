namespace AdventOfCode2021
open Utils
module Day11 =
    let runFlashStep arr =
        let kernels = Array2D.kernel 3 3 arr |> Array2D.map (Array2D.map (Option.defaultValue -1))
        kernels |> Array2D.map (fun kernel ->
            let value = kernel.[1,1]
            match value with
            | -1 -> (false, -1)
            | n when n >= 10 -> (true, -1)
            | _ ->
                let blinkingNeighbors = Array2D.toSeq kernel |> Seq.where ((<) 9) |> Seq.length
                (false, value + blinkingNeighbors)
        )

    let rec runFlashes flashesSoFar arr =
        let output = runFlashStep arr
        let flashCount = output |> Array2D.toSeq |> Seq.where fst |> Seq.length |> uint64
        let outputData = output |> Array2D.map snd
        if flashCount = 0UL then flashesSoFar, outputData
        else runFlashes (flashesSoFar + flashCount) outputData

    let runStep arr =
        let count,arr = Array2D.map ((+)1) arr |> runFlashes 0UL
        count, Array2D.map (fun v -> if v = -1 then 0 else v) arr

    let run lines =
        let NUM_STEPS = 100
        let board = array2D(lines |> List.map (fun str -> str |> Seq.map string |> Seq.map int |> List.ofSeq))
        [0..NUM_STEPS-1] |> List.fold (fun (count,state) arr -> 
            let stepCount,state = runStep state
            (count + stepCount, state)
        ) (0UL, board) |> fst

    let rec runUntilRepeat arr tgtFlash index =
        let flashCount,state = runStep arr
        if flashCount = tgtFlash 
            then index
            else runUntilRepeat state tgtFlash (index+1UL)

    let run2 lines =
        let board = array2D(lines |> List.map (fun str -> str |> Seq.map string |> Seq.map int |> List.ofSeq))
        let boardSize = uint64 (Array2D.length1 board * Array2D.length2 board)
        runUntilRepeat board boardSize 1UL

module Day12 =

    type Cave = 
        | Small of string
        | Big of string

    let terminalNodeName = Small "end"


    let nameOf cave = match cave with | Small s | Big s -> s

    let isBigCave name = name |> Seq.forall

    let isStepValid seenSteps nextStep =
        Set.contains nextStep seenSteps |> not

    let isSmall cave = match cave with | Small _ -> true | Big _ -> false
    
    let rec findPaths (connections:Map<Cave, Cave list>) (pathSoFar: Cave list) (seenAlready: Cave Set) (currentNode: Cave) : Cave list list =
        if currentNode = terminalNodeName 
        then
            [pathSoFar]
        else
            connections.[currentNode] |>
            List.where (isStepValid seenAlready) |>
            List.collect (fun step ->
                let newPathSoFar = pathSoFar@[step]
                match step with
                | Big _ -> findPaths connections newPathSoFar seenAlready step
                | Small v -> findPaths connections newPathSoFar (Set.add step seenAlready) step)
            
    let run lines =
        let connections = 
            lines |> 
            List.fold (fun map rule -> 
                let [caveA;caveB] = String.split "-" rule |> List.map (fun s -> if s.ToUpperInvariant() = s then Big s else Small s)
                map |>
                Map.change caveA (Option.defaultValue [] >> List.append [caveB] >> Some) |>
                Map.change caveB (Option.defaultValue [] >> List.append [caveA] >> Some)
            ) Map.empty
        let startNode = Small "start"
        let allPaths = findPaths connections [startNode] (Set.ofList [startNode]) startNode
        printfn "%A" (allPaths |> List.map (fun path -> path |> List.map nameOf |> String.concat ", "))
        allPaths |> List.length

        
    let isStepValid2 seenSteps canDoubleMove nextStep =
        if Set.contains nextStep seenSteps |> not
        then true
        else if canDoubleMove && nextStep <> Small "start" then true else false
    
    let rec findPaths2 (connections:Map<Cave, Cave list>) (pathSoFar: Cave list) (seenAlready: Cave Set) (canDoubleMove: bool) (currentNode: Cave) : Cave list list =
        if currentNode = terminalNodeName 
        then
            [pathSoFar]
        else
            connections.[currentNode] |>
            List.where (isStepValid2 seenAlready canDoubleMove) |>
            List.collect (fun step ->
                let newPathSoFar = pathSoFar@[step]
                match step with
                | Big _ -> findPaths2 connections newPathSoFar seenAlready canDoubleMove step
                | Small _ -> 
                    if Set.contains step seenAlready 
                    then findPaths2 connections newPathSoFar (Set.add step seenAlready) false step
                    else findPaths2 connections newPathSoFar (Set.add step seenAlready) canDoubleMove step)
    
    let run2 lines =
        let connections = 
            lines |> 
            List.fold (fun map rule -> 
                let [caveA;caveB] = String.split "-" rule |> List.map (fun s -> if s.ToUpperInvariant() = s then Big s else Small s)
                map |>
                Map.change caveA (Option.defaultValue [] >> List.append [caveB] >> Some) |>
                Map.change caveB (Option.defaultValue [] >> List.append [caveA] >> Some)
            ) Map.empty
        let startNode = Small "start"
        let allPaths = findPaths2 connections [startNode] (Set.ofList [startNode]) true startNode
        printfn "%A" (allPaths |> List.map (fun path -> path |> List.map nameOf |> String.concat ", "))
        allPaths |> List.length

module Day13 =

    // fold UP at y = h
    let foldHorizontal h (x,y) =
        if y <= h then (x,y) else (x, 2*h - y)

    // fold LEFT at x = w
    let foldVertical w (x,y) =
        if x <= w then (x,y) else (2*w - x, y)

    let runFold pts fold =
        pts |> Seq.fold (fun set pt -> Set.add (fold pt) set) Set.empty

    let run lines =
        let dots = 
            lines |> 
            List.takeWhile (String.isEmptyOrWhitespace >> not) |> 
            List.map (String.split ",") |>
            List.map (fun [x;y] -> (int x, int y))

        let instructions = 
            lines |> 
            List.skipWhile (String.isEmptyOrWhitespace >> not) |> 
            List.skip 1 |>
            List.map (fun s -> 
                let v = int s.[13..]
                match s.[11] with
                | 'x' -> foldVertical v 
                | 'y' -> foldHorizontal v
                | _ -> failwith "Invalid axis")

        let firstFold = List.head instructions

        runFold dots firstFold |> Seq.length

    let dotsToString dots =
        let maxX = dots |> Seq.map fst |> Seq.max
        let maxY = dots |> Seq.map snd |> Seq.max
        System.String ([|for y in 0..maxY do yield! [for x in 0..maxX do if Set.contains (x,y) dots then '#' else '.']; yield '\n'|])

    let run2 lines =
        let dots = 
            lines |> 
            List.takeWhile (String.isEmptyOrWhitespace >> not) |> 
            List.map (String.split ",") |>
            List.map (fun [x;y] -> (int x, int y))

        let instructions = 
            lines |> 
            List.skipWhile (String.isEmptyOrWhitespace >> not) |> 
            List.skip 1 |>
            List.map (fun s -> 
                let v = int s.[13..]
                match s.[11] with
                | 'x' -> foldVertical v 
                | 'y' -> foldHorizontal v
                | _ -> failwith "Invalid axis")

        let dots = List.fold (fun sq fold -> runFold sq fold) (Set.ofSeq dots) instructions
        dotsToString dots

module Day14 =

    let stepPolymer (polymer: char list) (rules: Map<char*char,char>) =
        (polymer.[0])::(polymer |>
            List.windowed 2 |>
            List.collect (fun [prev; next] ->
                match Map.tryFind (prev,next) rules with
                | None -> [next]
                | Some c -> [c;next]
            ))

    let run lines =
        let rules = lines |> List.skip 2 |> List.map (String.split " -> ") |> List.map (fun [key;value] -> (key.[0], key.[1]),value.[0]) |> Map.ofSeq
        let initial = List.ofSeq lines.[0]
        let result = [0..40-1] |> List.fold (fun polymer _ -> stepPolymer polymer rules) initial
        let counts = result |> List.countBy id
        let maxCount = counts |> List.map snd |> List.max
        let minCount = counts |> List.map snd |> List.min
        maxCount - minCount

    let stepPolymerGroup (((a,b),count):(char*char)*uint64) (rules: Map<char*char,char>) =
        Map.tryFind (a,b) rules |> Option.map (fun resultChar -> [(a,resultChar),count; (resultChar,b),count]) |> Option.defaultValue [((a,b),count)]

    let stepPolymer2 (polymer: Map<char*char,uint64>) (rules: Map<char*char,char>) =
        Seq.fold (fun result nextPolymerGroup -> 
            let outputGroups = stepPolymerGroup nextPolymerGroup rules
            List.fold (fun result ((a,b),count) -> Map.change (a,b) (fun v -> Option.defaultValue 0UL v + count |> Some) result) result outputGroups
        ) Map.empty (Map.toSeq polymer)

    let run2 lines =
        let rules = lines |> List.skip 2 |> List.map (String.split " -> ") |> List.map (fun [key;value] -> (key.[0], key.[1]),value.[0]) |> Map.ofSeq
        let initial = List.ofSeq ("$" + lines.[0] + "$") |> List.windowed 2 |> Seq.map (fun [a;b] -> (a,b)) |> Seq.countBy id |> Seq.map (fun (ch,ct) -> ch,uint64 ct) |> Map.ofSeq
        let result = [0..40-1] |> List.fold (fun polymer _ -> stepPolymer2 polymer rules) initial
        let charQtys = result |> Map.toSeq |> Seq.collect (fun ((a,b),count) -> [(a,count);(b,count)]) |> Seq.groupBy fst |> Seq.map (fun (key, values) -> (key, (Seq.map snd values |> Seq.sum) / 2UL)) |> Map.ofSeq |> Map.remove '$'
        let maxCount = Map.toSeq charQtys |> Seq.map snd |> Seq.max
        let minCount = Map.toSeq charQtys |> Seq.map snd |> Seq.min
        maxCount - minCount

module Day15 =
    
    type PathfinderNode = { parent: PathfinderNode Option; location: (int*int); pathScore: uint64 }

    let defaultGetMoveCost _ nextStep world = nextStep ||> Array2D.get world |> uint64
    
    let insertSorted item list =
        let targetIndex = List.tryFindIndex (fun otherItem -> otherItem.pathScore > item.pathScore) list
        match targetIndex with
        | None -> list @ [item]
        | Some i -> List.insertAt i item list

    let mergeWithPendingSet (next:PathfinderNode) (pendingSet: PathfinderNode list) (closedSet:Map<(int*int),PathfinderNode>) =
        let ({ location= nextLocation }) = next
        if Map.containsKey nextLocation closedSet 
        then pendingSet
        else
            let pendingItem = List.tryFind (fun node -> node.location = nextLocation) pendingSet
            match pendingItem with
            | None -> insertSorted next pendingSet
            | Some oldItem ->
                if oldItem.pathScore <= next.pathScore 
                then pendingSet 
                else List.removeAt (List.findIndex ((=)oldItem) pendingSet) pendingSet |> insertSorted next

    let rec findPath (world:int[,]) (getMoveCost:PathfinderNode -> int*int -> int[,] -> uint64) (pendingSet: PathfinderNode list) (closedSet:Map<(int*int),PathfinderNode>) =
        match pendingSet with
        | [] -> None
        | parent::rest ->
            match parent with
            | { location = loc } when loc = (Array2D.length1 world - 1, Array2D.length2 world - 1) ->
                Some parent // Path done!
            | { location = loc; } ->
                let x,y = loc
                let neighborCoords = seq { for dx in [-1;1] do yield! [(dx+x,y); (x,dx+y)]} |> Seq.where (fun (x,y) -> x >= 0 && y >= 0 && x < Array2D.length1 world && y < Array2D.length2 world)
                let nextSteps = Seq.map (fun index -> { parent = Some parent; location = index; pathScore = parent.pathScore + getMoveCost parent index world }) neighborCoords
                let newPendingSet = Seq.fold (fun pendingSet item -> mergeWithPendingSet item pendingSet closedSet) rest nextSteps
                //printfn "%A -> %A" loc (nextSteps |> Seq.map (fun ({location = loc; pathScore = score}) -> sprintf "%A[%i]" loc score))
                findPath world getMoveCost newPendingSet (Map.add parent.location parent closedSet)
            
    
    let djikstraSearch world =
        findPath world defaultGetMoveCost [{ parent = None; location = (0,0); pathScore = 0UL }] Map.empty

    let run lines =
        let world = lines |> List.map (Seq.map string >> Seq.map int) |> array2D
        match djikstraSearch world with
        | None -> failwith "No path??"
        | Some lastNode -> lastNode.pathScore

    let rec findPath2 (world:int[,]) (getMoveCost:PathfinderNode -> int*int -> int[,] -> uint64) (target:int*int) (pendingSet: PathfinderNode list) (closedSet:Map<(int*int),PathfinderNode>) =
        let tx,ty = target
        match pendingSet with
        | [] -> None
        | parent::rest ->
            match parent with
            | { location = loc } when loc = target ->
                Some parent // Path done!
            | { location = loc; } ->
                let x,y = loc
                let neighborCoords = seq { for dx in [-1;1] do yield! [(dx+x,y); (x,dx+y)]} |> Seq.where (fun (x,y) -> x >= 0 && y >= 0 && x <= tx && y <= ty)
                let nextSteps = Seq.map (fun index -> { parent = Some parent; location = index; pathScore = parent.pathScore + getMoveCost parent index world }) neighborCoords
                let newPendingSet = Seq.fold (fun pendingSet item -> mergeWithPendingSet item pendingSet closedSet) rest nextSteps
                //printfn "%A -> %A" loc (nextSteps |> Seq.map (fun ({location = loc; pathScore = score}) -> sprintf "%A[%i]" loc score))
                findPath2 world getMoveCost target newPendingSet (Map.add parent.location parent closedSet)
    
    let getMoveCost2 _ (x,y) world = 
        let tx,ty = (x % (Array2D.length1 world), y % (Array2D.length2 world))
        let worldDelta = (x / (Array2D.length1 world)) + (y / (Array2D.length2 world))
        ((((Array2D.get world tx ty) + worldDelta) - 1) % 9) + 1 |> uint64

        
    let djikstraSearch2 world =
        findPath2 world getMoveCost2 (Array2D.length1 world * 5 - 1, Array2D.length2 world * 5 - 1) [{ parent = None; location = (0,0); pathScore = 0UL }] Map.empty
            
     
    let run2 lines =
        let world = lines |> List.map (Seq.map string >> Seq.map int) |> array2D
        match djikstraSearch2 world with
        | None -> failwith "No path??"
        | Some lastNode -> lastNode.pathScore