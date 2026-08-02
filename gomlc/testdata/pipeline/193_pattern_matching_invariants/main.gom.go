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
    var t236 int32
    var inline265 int32 = 12
    t236 = inline265
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t236)
    _goml_runtime_core_string_println(inline261)
    var t237 int32
    var inline259 bool = true
    switch inline259 {
    case true:
        t237 = 1
    case false:
        t237 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t237)
    _goml_runtime_core_string_println(inline256)
    var t238 string
    var inline254 float64 = 0
    switch inline254 {
    case -0:
        t238 = "zero"
    default:
        t238 = "other"
    }
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline251)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t247 string = _goml_runtime_core_int32_to_string(self__43)
    return t247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
