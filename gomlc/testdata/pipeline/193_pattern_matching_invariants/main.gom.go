package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Never struct {
    items []Never
}

type _goml_vec_bool struct {
    items []bool
}

type Tuple1_6Single struct {
    _0 Single
}

type Never int32

const (

)

type Loop interface {
    isLoop()
}

type Next struct {
    _0 Loop
}

func (_ Next) isLoop() {}

type MaybeNever interface {
    isMaybeNever()
}

type Empty struct {}

func (_ Empty) isMaybeNever() {}

type Filled struct {
    _0 Never
}

func (_ Filled) isMaybeNever() {}

type Single interface {
    isSingle()
}

type Only struct {
    _0 int32
}

func (_ Only) isSingle() {}

func main0() struct{} {
    var t217 int32
    var inline246 int32 = 12
    t217 = inline246
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
    _goml_runtime_core_string_println(inline242)
    var t218 int32
    var inline240 bool = true
    switch inline240 {
    case true:
        t218 = 1
    case false:
        t218 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t218)
    _goml_runtime_core_string_println(inline237)
    var t219 string
    var inline235 float64 = 0
    switch inline235 {
    case -0:
        t219 = "zero"
    default:
        t219 = "other"
    }
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline232)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t228 string = _goml_runtime_core_int32_to_string(self__72)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
