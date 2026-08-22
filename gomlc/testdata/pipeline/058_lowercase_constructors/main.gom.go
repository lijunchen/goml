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

type Maybe__i32 struct {
    _tag int32
    _v0_0 int32
}

func main0() int32 {
    var commute_field834 int32
    var inline832 int32 = 5
    commute_field834 = inline832
    var pt__7 Point
    var inline829 int32 = 7
    var inline830 Point = Point{
        x: commute_field834,
        y: inline829,
    }
    pt__7 = inline830
    var t816 int32 = pt__7.x
    var t819 int32
    var inline823 int32 = pt__7.x
    var inline824 int32 = pt__7.y
    var inline827 int32 = inline823 + inline824
    t819 = inline827
    var t820 int32 = t816 + t819
    return t820
}

func main() {
    main0()
}
