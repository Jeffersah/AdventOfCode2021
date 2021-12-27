namespace AdventOfCode2021
module Utils =

    module Tuples =
        let first (a,_) = a
        let second (_,a) = a
        let first3 (a,_,_) = a
        let second3 (_,a,_) = a
        let third3 (_,_,a) = a
        let flip (a,b) = (b,a)
        let map mapFn (a,b) = (mapFn a, mapFn b)
        let mapFirst mapFn (a, b) = (mapFn a, b)
        let mapSecond mapFn (a, b) = (a, mapFn b)
        let from a b = (a,b)
        let from3 a b c = (a,b,c)

    module Array2D =
        let toSeq arr =
            seq {
                for y in [0..(Array2D.length2 arr) - 1] do
                    for x in [0..(Array2D.length1 arr) - 1] do
                        yield Array2D.get arr x y
            }

        let tryGet arr x y =
            if x < 0 || y < 0 || x >= Array2D.length1 arr || y >= Array2D.length2 arr then None
            else Some (Array2D.get arr x y)

        let neighbors arr x y =
            seq { for dx in [-1..1] do for dy in [-1..1] do if dx <> 0 || dy <> 0 then yield tryGet arr (x+dx) (y+dy) } |>
            Seq.choose id

        let neighborIndecies arr x y =
            seq { for dx in [-1..1] do for dy in [-1..1] do if (dx <> 0 || dy <> 0) && (tryGet arr (x+dx) (y+dy) |> Option.isSome) then yield (x+dx),(y+dy) }
        
        let orthoNeighbors arr x y =
            seq { for dx in [-1..1] do for dy in [-1..1] do if (dx <> 0 || dy <> 0) && (abs(dx)+abs(dy) = 1) then yield tryGet arr (x+dx) (y+dy) } |>
            Seq.choose id

        let orthoNeighborIndecies arr x y =
            seq { for dx in [-1..1] do for dy in [-1..1] do if (dx <> 0 || dy <> 0) && (abs(dx)+abs(dy) = 1) && (tryGet arr (x+dx) (y+dy) |> Option.isSome) then yield (x+dx),(y+dy) }

        let fold<'TState,'T> folder (state:'TState) (arr:'T[,]) =
            let coords = seq { for x in [0..Array2D.length1 arr - 1] do for y in [0..Array2D.length2 arr - 1] do x,y }
            Seq.fold (fun s (x,y) -> folder s (arr.[x,y])) state coords

        let foldi<'TState,'T> folder (state:'TState) (arr:'T[,]) =
            let coords = seq { for x in [0..Array2D.length1 arr - 1] do for y in [0..Array2D.length2 arr - 1] do x,y }
            Seq.fold (fun s (x,y) -> folder s x y (arr.[x,y])) state coords
            
        let flatMap map arr =
            seq { for y in [0..Array2D.length2 arr - 1] do for x in [0..Array2D.length1 arr - 1] do yield Array2D.get arr x y |> map}

        let flatMapi map arr =
            seq { for y in [0..Array2D.length2 arr - 1] do for x in [0..Array2D.length1 arr - 1] do yield Array2D.get arr x y |> map x y}

        let slice arr x y w h =
            Array2D.init w h (fun dx dy -> Array2D.get arr (x+dx) (y+dy))

        let size arr = Array2D.length1 arr, Array2D.length2 arr

        let coords arr =
            let w,h = size arr
            [for x in 0..w-1 do for y in 0..h-1 do (x,y)]

        let kernel width height arr =
            let w, h = size arr
            let dx, dy = width / 2, height / 2
            Array2D.init w h (fun x y ->
                Array2D.init width height (fun kx ky ->
                    let x,y = x + kx - dx, y + ky - dy
                    if x < 0 || y < 0 || x >= w || y >= h then None else Array2D.get arr x y |> Some 
                )
            )

        let kernelInside width height arr =
            let w, h = size arr
            let dx, dy = width / 2, height / 2
            Array2D.init (w-width+1) (h-height+1) (fun x y ->
                Array2D.init width height (fun kx ky ->
                    let x,y = x + kx, y + ky
                    Array2D.get arr x y
                )
            )

        let border xAmount yAmount arr padValue =
            let w,h = size arr
            Array2D.init (xAmount * 2 + w) (yAmount * 2 + h) (fun x y ->
                let tx = x - xAmount
                let ty = y - yAmount
                if tx < 0 || ty < 0 || tx >= w || ty >= h then padValue else Array2D.get arr tx ty
            )

        let rows (board:'a[,]):('a list list) =
            let w,h = size board
            [for y in [0..(h-1)] do 
                [for x in [0..(w - 1)] do 
                    Array2D.get board x y]]

        let transpose board =
            let w,h = size board
            Array2D.init h w (fun x y -> Array2D.get board y x)

        let fill value arr =
            Array2D.iteri (fun x y _ -> Array2D.set arr x y value) arr


    module String =
        let split (seperator:string) (str:string) =
            str.Split (seperator) |> List.ofArray
            
        let splitWithOptions (seperator:string) options (str:string) =
            str.Split (seperator, options) |> List.ofArray

        let isEmpty (str: string) =
            str = null || str = ""

        let isEmptyOrWhitespace (str:string) =
            isEmpty str || str |> Seq.forall System.Char.IsWhiteSpace

        let contains (haystack:string) (needle:string) =
            haystack.IndexOf(needle) <> -1

        let indexOf (haystack:string) (needle:string) =
            haystack.IndexOf(needle)

        let padRight (length: int) (padChar: char) (input: string) =
            let padQty = length - String.length input |> max 0
            if padQty = 0 then input 
            else
                let padding = System.String(padChar, padQty)
                input + padding


    module List =
        let splitOn param list =
            let rec splitOnRecurse param list soFar =
                match list with
                | [] ->
                    if soFar = [] then [] else [soFar]
                | f::r ->
                    if param f then
                        soFar::(splitOnRecurse param r [f])
                    else
                        splitOnRecurse param r (soFar @ [f])
            splitOnRecurse param list []
            
        let replace i v (list:'a list) =
            list.[..i-1] @ (v :: list.[i+1..])

        let tryTake i l =
            List.take (min i (List.length l)) l
            
        let trySkip i l =
            List.skip (min i (List.length l)) l

        let trySplitAt i l =
            tryTake i l, trySkip i l

        let tryGet i l = 
            if i >= List.length l || i < 0 then None else Some l.[i]


    let cache f =
        let cache = new System.Collections.Generic.Dictionary<_,_>()
        fun c ->
            match (cache.TryGetValue c) with
            | true, value -> value
            | false, _ -> 
                let result = f c
                cache.Add(c, result)
                result

    module Regex =
        let groups regex string =
           let rgx = System.Text.RegularExpressions.Regex(regex)
           let rmatch = rgx.Match(string)
           if not rmatch.Success then None
           else Some(rmatch.Groups |> Seq.skip 1 |> Seq.map (fun grp -> if grp.Success then Some(grp.Value) else None) |> Seq.toList)

        let (|Regex|_|) rgx str = groups rgx str