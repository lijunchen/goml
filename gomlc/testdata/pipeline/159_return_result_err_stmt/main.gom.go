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

type _goml_m_Result_____o__q_____string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

func main0() struct{} {
    return struct{}{}
}

func main() {
    main0()
}
