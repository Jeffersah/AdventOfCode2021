namespace AdventOfCode2021
open Utils

module Day16 =
    
    type PacketHeader = { version: int; typeID: int }
    type Packet = 
        | Literal of PacketHeader * uint64
        | Operator of PacketHeader * Packet list

    let hexToByte ch =
        if ch >= '0' && ch <= '9' 
        then byte (string (ch))
        else 
            let ch = byte ch
            ch - (byte 'A') + 10uy

    let bitsOf byte =
        [ (byte &&& 8uy) > 0uy; (byte &&& 4uy) > 0uy; (byte &&& 2uy) > 0uy; (byte &&& 1uy) > 0uy ]

    let hexToBits str =
        str |> Seq.map hexToByte |> Seq.collect bitsOf |> List.ofSeq

    let bitsToValue bits =
        let rec bitsToValueTailRecurse bits sum =
            match bits with
            | [] -> sum
            | f::r ->
                (if f then 1UL else 0UL) |> ((+) (sum * 2UL)) |> bitsToValueTailRecurse r
        bitsToValueTailRecurse bits 0UL

    let parseHeader bits =
        let ver, bits = List.splitAt 3 bits
        let typ, bits = List.splitAt 3 bits
        { version = int(bitsToValue ver); typeID = int(bitsToValue typ) }, bits

    let parseLiteral bits header =
        let rec readValue bits value =
            let segment, bits = List.splitAt 5 bits
            let hasMoreSegments::segment = segment
            let segment = bitsToValue segment
            if not hasMoreSegments
            then value + segment, bits
            else readValue bits ((value + segment) <<< 4)
        let value,bits = readValue bits 0UL
        Literal (header,value), bits

    let rec parseOp bits header =
        let lengthType::bits = bits
        let lengthBits,bits = List.splitAt (if lengthType then 11 else 15) bits
        let opLength = bitsToValue lengthBits
        let subPackets, subBits =
            if lengthType
            then 
                parseNPackets bits opLength
            else 
                let subBits,bits = List.splitAt (int opLength) bits
                (parseAllPackets subBits), bits
        Operator (header, subPackets), subBits
        
    and parseAllPackets bits =
        match bits with
        | [] -> []
        | _ -> 
            let p,b = parsePacket bits
            p::(parseAllPackets b)

    and parseNPackets bits packetCount =
        if packetCount = 0UL 
        then [],bits
        else 
            let p,b = parsePacket bits
            let rest,b = parseNPackets b (packetCount - 1UL)
            p::rest,b

    and parsePacket bits =
        let header, bits = parseHeader bits
        if header.typeID = 4 
        then parseLiteral bits header
        else parseOp bits header


    let run (lines: string list) =
        lines |> List.map (fun line -> 
            let bits = hexToBits line
            let packet, bits = parsePacket bits
            printfn "%A" packet
            let rec sumVersions packet =
                match packet with
                | Literal (header,_) -> header.version
                | Operator (header, children) -> header.version + (List.map sumVersions children |> List.sum)
            sumVersions packet
        )

    let rec eval packet =
        match packet with
        | Literal (_, v) -> v
        | Operator (header, children) ->
            let children = children |> List.map eval
            match header.typeID with
            | 0 -> List.sum children
            | 1 -> List.fold (*) 1UL children
            | 2 -> List.min children
            | 3 -> List.max children
            | 5 -> if children.[0] > children.[1] then 1UL else 0UL
            | 6 -> if children.[0] < children.[1] then 1UL else 0UL
            | 7 -> if children.[0] = children.[1] then 1UL else 0UL
            | _ -> failwithf "Unknown type %i " header.typeID

    let run2 (lines: string list) =
        lines |> List.map (fun line -> 
            let bits = hexToBits line
            let packet, bits = parsePacket bits
            printfn "%A" packet
            let rec sumVersions packet =
                match packet with
                | Literal (header,_) -> header.version
                | Operator (header, children) -> header.version + (List.map sumVersions children |> List.sum)
            eval packet
        )

module Day17 =
    
    let test target velocity =
        let rec testRec target (vx, vy) (x, y) =
            let (minX, maxX, minY, maxY) = target
            let xInRange,yInRange = x >= minX && x <= maxX, y >= minY && y <= maxY
            if xInRange && yInRange then true
            else
                // Failure conditions:
                // 1: We're too low and we're moving downard.
                // 2: We're not in range (X) and we're moving the wrong way
                let xDir, yDir = minX - x, minY - y
                if (not yInRange && vy < 0 && yDir > 0) 
                    then false 
                else if (not xInRange && sign(xDir) <> sign(vx)) 
                    then false
                else 
                    let x,y = x + vx, y + vy
                    let vx,vy = vx - (1 * sign vx), vy - 1
                    testRec target (vx, vy) (x,y)
        testRec target velocity (0,0)

    let yCoords yVelocity minY =
        Seq.unfold (fun (p, v) -> 
            if p < minY then None else
            Some(p,(p+v, v-1))
        ) (0, yVelocity)

    let xCoords xVelocity =
        Seq.unfold (fun (p, v) -> 
            if v = 0 
            then None 
            else Some(
                p, 
                (p + v, v - (1 * sign v))
            )
        ) (0, xVelocity)

    let findInterceptRangeX (xMin, xMax) velocity =
        let rec findInterceptTailRecurse (xMin, xMax) x v tNow tIntersectStart =
            match v = 0, tIntersectStart, (x >= xMin && x <= xMax) with
            | true, _, true -> Some (Option.defaultValue tNow tIntersectStart, None)
            | true, Some start, false -> Some(start, Some(tNow - 1))
            | true, None, false -> None
            | false, None, true -> findInterceptTailRecurse (xMin, xMax) (x+v) (v-1) (tNow + 1) (Some tNow)
            | false, None, false -> findInterceptTailRecurse (xMin, xMax) (x+v) (v-1) (tNow + 1) None
            | false, _, true -> findInterceptTailRecurse (xMin, xMax) (x+v) (v-1) (tNow + 1) tIntersectStart
            | false, Some start, false -> Some(start, Some(tNow - 1))
        findInterceptTailRecurse (xMin, xMax) 0 velocity 0 None

    // Finds stepcounts of all intersections, in descending (best first) order.
    let findYIntercepts (yMin, yMax) coords =
        coords |>
            Seq.mapi (fun stepNumber coord -> stepNumber, coord) |>
            Seq.choose (fun (stepNumber, coord) -> 
                if coord >= yMin && coord <= yMax then Some stepNumber else None
            ) |>
            Seq.rev


    let validXVelocities (minX, maxX) =
        let maxXVelocity = maxX
        [1..maxXVelocity] |>
            List.choose (fun v ->
                match findInterceptRangeX (minX, maxX) v with
                | None -> None
                | Some rng -> Some (v, rng)
            )

    let run lines =
        lines |> List.map(fun targetString ->
            let [xMin;xMax;yMin;yMax] = Regex.groups @".*x=(-?\d+)\.\.(-?\d+),.*y=(-?\d+)\.\.(-?\d+)" targetString |> Option.get |> List.map Option.get |> List.map int
            yCoords (abs(yMin) - 1) yMin |> Seq.max
        )

    let inRange range value =
        match range with
        | (min, None) -> value >= min
        | (min, Some max) -> value >= min && value <= max

    let run2 lines =
        lines |> List.map(fun targetString ->
            let [xMin;xMax;yMin;yMax] = Regex.groups @".*x=(-?\d+)\.\.(-?\d+),.*y=(-?\d+)\.\.(-?\d+)" targetString |> Option.get |> List.map Option.get |> List.map int
            let xRanges = validXVelocities (xMin, xMax) |> List.map snd
            let yIntercepts = [yMin..(abs(yMin) - 1)] |> List.map (fun v -> findYIntercepts (yMin, yMax) (yCoords v yMin))
            yIntercepts |> List.map (fun ySeq ->
                    xRanges |> Seq.where (fun r -> Seq.exists (inRange r) ySeq) |> Seq.length
                ) |> List.sum
        )
        
module Day18 =

    type SNumber = | Value of int | Tuple of SNumber*SNumber
    
    let rec toString v =
        match v with
        | Value v -> string v
        | Tuple (a,b) -> sprintf "[%s,%s]" (toString a) (toString b)

    let splitVal v =
        if v % 2 = 0 then Tuple (Value (v/2), Value (v/2))
        else Tuple(Value (v/2), Value ((v+1)/2))

    let rec trySplit num =
        match num with
        | Value v when v >= 10 -> Ok (splitVal v)
        | Value v -> Error (Value v)
        | Tuple (a, b) ->   
            match (trySplit a, trySplit b) with
            | Ok(result), _ -> Ok (Tuple(result, b))
            | _, Ok(result) -> Ok (Tuple(a, result))
            | _,_ -> Error num

    let rec addLeft add snum =
        match snum with
        | Value v -> Value (v+add)
        | Tuple (a, b) -> Tuple (addLeft add a, b)
        
    let rec addRight add snum =
        match snum with
        | Value v -> Value (v+add)
        | Tuple (a, b) -> Tuple (a, addRight add b)

    // No explosion:        Err (original value)
    //    any explosion:    Ok (newValue, carryLeft option, carryRight option)
    let rec tryExplode num depth =
        match num, depth with
        | Value _, _ -> Error num 
        | Tuple (Value a, Value b), d when d>=4 ->
            Ok(Value 0, Some a, Some b)
        | Tuple (l, r), _ ->
            match tryExplode l (depth+1), tryExplode r (depth+1) with
            | Ok (newV, cl, cr), _ -> 
                let newRightBranch = match cr with
                                     | None -> r
                                     | Some v -> addLeft v r
                Ok (Tuple (newV, newRightBranch), cl, None)
            | _, Ok (newV, cl, cr) ->
                let newLeftBranch = match cl with
                                     | None -> l
                                     | Some v -> addRight v l
                Ok (Tuple (newLeftBranch, newV), None, cr)
            | Error _, Error _ -> Error num
        

    let rec reduce snumber =
        //printfn "Reduce %s" (toString snumber)
        match tryExplode snumber 0, trySplit snumber with 
        | Ok (newV, _, _), _ ->
            //printfn "...Expl %s" (toString newV)
            reduce newV 
        | _, Ok(newV) -> 
            //printfn "...Spl %s" (toString newV)
            reduce newV 
        | _, _ -> snumber

    let rec addsn a b = 
        let result = Tuple (a,b) |> reduce
        //printfn "%s" (toString result)
        result

    let rec findComma (line:string) depth =
        match line.[0], depth with
        | ',', 0 -> 0
        | '[', _ -> 1 + findComma line.[1..] (depth + 1)
        | ']', _ -> 1 + findComma line.[1..] (depth - 1)
        | _, _ -> 1 + findComma line.[1..] depth

    let rec parse (line:string) =
        if line.[0] <> '[' 
        then int line |> Value
        else
            let inside = line.[1..String.length (line) - 2]
            let commaIndex = findComma inside 0
            Tuple (parse inside.[..commaIndex - 1], parse inside.[commaIndex + 1..])

    let rec magnitude sn =
        match sn with
        | Value v -> uint64 v
        | Tuple(a,b) -> 
            3UL * magnitude a + 2UL * magnitude b

    let run lines =
        lines |> List.map parse |> List.reduce addsn |> magnitude

    let rec run2 lines =
        let nums = lines |> List.map parse
        List.allPairs nums nums |> List.where (fun (a,b) -> a<>b) |> List.map (fun (a,b) -> magnitude (addsn a b)) |> List.max

module Day19 =
    open Point3

    let NUM_PTS = 12
    let NUM_COMBINATIONS = 66

    type Basis = Point*Point
    type KnownEntry = (Point list)*(Basis)*(Point)

    let distanceFunction a b = Point.lenSq (a - b)

    let worldToBasis (basis:Basis) pt =
        let basisX, basisY = basis
        let basisZ = basisX *** basisY
        Point(basisX .* pt, basisY .* pt, basisZ .* pt)

    let basisToWorld (basis:Basis) (pt:Point) =
        let basisX, basisY = basis
        let basisZ = basisX *** basisY
        (basisX * pt.x) + (basisY * pt.y) + (basisZ * pt.z)

    let findGlobalBasis (basis: Basis) (relativeTo: Basis) =
        let fwd,up = basis
        basisToWorld relativeTo fwd, basisToWorld relativeTo up

    let rec allBasies =
        [for fwd in [0;1;2] do for up in (List.except [fwd] [0;1;2]) do
            for fd in [-1;1] do for yd in [-1; 1] do
                Point.withAxis fwd fd (Point(0,0,0)), Point.withAxis up yd (Point(0,0,0))]

    let toDistanceSet (coords: Point list) =
        List.allPairs coords coords |> List.where (fun (a,b) -> a<>b) |> List.map ((<||)distanceFunction) |> Set.ofSeq

    let findCommonDistances (coordSets: Point list list) =
        List.map toDistanceSet coordSets |> List.reduce (Set.intersect)

    let findLargestIntersection coordsA coordsB =
        let dists = findCommonDistances [coordsA;coordsB]
        dists |> Seq.max

    let findPointsWithGivenDistance dist coordset =
        let len = List.length coordset
        [for i in 0..len-1 do 
            for j in (i+1)..(len-1) do
                if distanceFunction coordset.[i] coordset.[j] = dist then yield (coordset.[i], coordset.[j])]

    let rec parse inputLines = 
        let parseCoordSet lines =
            let coordLines = List.takeWhile (String.isEmptyOrWhitespace >> not) lines
            let restOfLines = List.skipWhile (String.isEmptyOrWhitespace >> not) lines |> List.skip 1
            let coords = coordLines |> List.where (fun s -> s.[0..2] <> "---") |> List.map (fun str ->
                let [x;y;z] = String.split "," str |> List.map int
                Point(x, y, z))
            coords, restOfLines
        match inputLines with
        | [] -> []
        | _ -> 
            let coords, rest = parseCoordSet inputLines
            coords::(parse rest)

    let isValidBasisPair relativeBasis ((p0,p1),(p2,p3)) =
        let d0 = p1-p0
        let d1 = (basisToWorld relativeBasis p3) - (basisToWorld relativeBasis p2)
        d0 = d1 || d0 = -d1

    let findRelativePosition (aPts:(Point*Point)list) (bPts:(Point*Point)list) =
        let p0, p1 = aPts.[0]
        let p2, p3 = bPts.[0]
        if (p1-p0) = (p3-p2) then p0 - p2
        else p0 - p3
        

    let tryFindRelativeOrientation a b : (Basis*Point) option =
        let dists = findCommonDistances [a;b]
        if Seq.length dists < NUM_COMBINATIONS then None
        else
            let commonPairs = dists |> Seq.choose (fun d -> 
                    let ap = findPointsWithGivenDistance d a
                    let bp = findPointsWithGivenDistance d b
                    match ap, bp with
                    | [va], [vb] -> Some (va, vb)
                    | _, _ -> None) |> List.ofSeq
            let relativeBasis = allBasies |> List.where (fun basis -> List.forall (isValidBasisPair basis) commonPairs)
            match relativeBasis with
            | [basis] ->
                let aPairs = commonPairs |> List.map fst
                let bPairs = commonPairs |> List.map snd |> List.map (fun (a,b) -> (basisToWorld basis a), (basisToWorld basis b))
                Some(basis, findRelativePosition aPairs bPairs)
            | _ -> None


    let tryAddToKnownList (knownList: KnownEntry list) (coordSet: Point list) =
        let hasRelativeOrientation = 
            knownList |> 
            List.choose (fun (kP, kB, kO) -> tryFindRelativeOrientation kP coordSet |> Option.map (fun (nB, nO) -> (kB, kO),(nB, nO))) |>
            List.tryHead
        match hasRelativeOrientation with
        | None -> None
        | Some((kB, kO), (nB, nO)) -> 
            let globalOrientation = findGlobalBasis nB kB
            let globalDeltaOffset = basisToWorld kB nO
            Some((coordSet, globalOrientation, kO + globalDeltaOffset)::knownList)

    let rec tryExpandKnownList (knownList: KnownEntry list) (coordSet: Point list list) =
        match coordSet with
        | [] -> knownList,[]
        | f::r -> 
            match tryAddToKnownList knownList f with
            | None -> 
                let k,r = tryExpandKnownList knownList r
                k, (f::r)
            | Some(newKnownList) -> 
                tryExpandKnownList newKnownList r

    let rec fillKnownList (knownList: KnownEntry list) (coordSet: Point list list) = 
        let newKnownList, newUnknownList = tryExpandKnownList knownList coordSet
        match newUnknownList with
        | [] -> newKnownList
        | _ -> if newKnownList.Length <> knownList.Length then fillKnownList newKnownList newUnknownList else failwith "Failed to expand known list"

    let globalBeacons ((coords, basis, offset): KnownEntry) =
        coords |> List.map (fun c -> (basisToWorld basis c) + offset)

    let run lines =
        let parsed = parse lines

        let knownList = [(parsed.[0], (Point(1, 0, 0), Point(0, 1, 0)), Point(0,0,0))]
        let unknownList = List.skip 1 parsed

        let knownList = fillKnownList knownList unknownList

        let allBeacons = knownList |> List.collect globalBeacons |> List.distinct
        allBeacons |> List.length

    let run2 lines =
        let parsed = parse lines

        let knownList = [(parsed.[0], (Point(1, 0, 0), Point(0, 1, 0)), Point(0,0,0))]
        let unknownList = List.skip 1 parsed

        let knownOffsets = fillKnownList knownList unknownList |> List.map (fun (_,_,o) -> o)
        List.allPairs knownOffsets knownOffsets |> List.map (fun (a,b) -> b-a) |> List.map Point.lenManhat |> List.max
        
module Day20 = 
    
    let indexFromKernel kernel =
        kernel |> Array2D.toSeq |> Seq.fold (fun v i -> (v <<< 1) ||| (if i then 1 else 0)) 0

    let iterate (key: bool[]) board =
        let newBoard = Array2D.kernelInside 3 3 board |> Array2D.map indexFromKernel |> Array2D.map (fun i -> key.[i])
        let topLeftValue = Array2D.get newBoard 0 0
        Array2D.border 2 2 newBoard topLeftValue

    let chToBool ch = if ch = '#' then true else false
    let boolToCh b = if b then '#' else '.'
    let parse (lines: string list) =
        let keyLine = lines.[0]
        let boardLines = lines.[2..]
        let key = keyLine |> Seq.map chToBool |> Array.ofSeq
        key, List.map (fun l -> l |> Seq.map chToBool |> Seq.toList) boardLines |> array2D |> Array2D.transpose

    let toString (board:bool[,]) =
        Array2D.rows board |> List.map (fun l -> List.map boolToCh l |> List.map string |> String.concat "") |> String.concat "\r\n"

    let rec iterN key board count =
        match count with
        | 0 -> board
        | n -> iterN (key) (iterate key board) (count - 1)

    let run lines =
        let key, board = parse lines
        let board = Array2D.border 3 3 board false
        iterN key board 50 |> Array2D.toSeq |> Seq.where id |> Seq.length