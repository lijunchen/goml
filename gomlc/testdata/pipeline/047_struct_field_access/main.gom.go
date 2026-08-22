package main

type _goml_vec_uint32 struct {
    items []uint32
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Point struct {
    x int32
    y int32
}

type Ordering int32

func main0() int32 {
    var p__2 Point
    var inline819 int32 = 5
    var inline820 int32 = inline819 + 1
    var inline821 Point = Point{
        x: inline819,
        y: inline820,
    }
    p__2 = inline821
    var t807 int32 = p__2.x
    var t808 int32 = t807 + 1
    var t809 int32 = p__2.y
    var t810 int32 = t809 - 2
    var t812 int32
    var inline817 int32 = t808 + t810
    t812 = inline817
    var t813 int32 = t808 + t812
    return t813
}

func main() {
    main0()
}
