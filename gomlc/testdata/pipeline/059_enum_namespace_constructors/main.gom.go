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

type Ordering int32

type Color int32

const (
    Color_Red Color = 0
    Green Color = 1
)

type Signal int32

const (
    Yellow Signal = 1
)

func main0() Signal {
    var current__2 Color = Color_Red
    switch current__2 {
    case Color_Red:
        return Yellow
    case Green:
        return Yellow
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
