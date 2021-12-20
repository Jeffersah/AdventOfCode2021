module Point3
type Point (x, y, z) =
    member this.x = x
    member this.y = y
    member this.z = z

    static member (+) (a: Point, b: Point) = Point(a.x + b.x, a.y + b.y, a.z + b.z)
    static member (-) (a: Point, b: Point) = Point(a.x - b.x, a.y - b.y, a.z - b.z)
    static member (~-) (a: Point) = Point(-a.x, -a.y, -a.z)
    /// Component-wise multiply or scalar product
    static member (*) (a: Point, b: Point) = Point(a.x * b.x, a.y * b.y, a.z * b.z)
    static member (*) (a: Point, s: int) = Point(a.x * s, a.y * s, a.z * s)
    static member (*) (s: int, b: Point) = b*s
    /// Dot product
    static member (.*) (a: Point, b: Point) = a.x * b.x + a.y * b.y + a.z * b.z
    /// Cross product
    static member ( *** ) (a:Point, b:Point) = Point(a.y*b.z - a.z * b.y, a.z*b.x-a.x*b.z, a.x*b.y-a.y*b.x)
    static member withAxis (axis: int) (value: int) (pt: Point) =
        Point((if axis = 0 then value else pt.x), (if axis = 1 then value else pt.y), (if axis = 2 then value else pt.z))
    static member lenSq (a : Point) = a .* a
    static member len (a: Point) = sqrt(float (Point.lenSq a))
    static member lenManhat (a: Point) = abs(a.x) + abs(a.y) + abs(a.z)
    override this.GetHashCode() =
        hash(this.x, this.y, this.z)
    override this.Equals(b) =
        match b with
        | :? Point as p -> (this.x, this.y, this.z) = (p.x, p.y, p.z)
        | _ -> false
    override this.ToString() =
        sprintf "(%i,%i,%i)" this.x this.y this.z