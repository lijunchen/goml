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
    var t253 int32
    var inline282 int32 = 12
    t253 = inline282
    var inline278 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t253)
    _goml_runtime_core_string_println(inline278)
    var t254 int32
    var inline276 bool = true
    switch inline276 {
    case true:
        t254 = 1
    case false:
        t254 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline273 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t254)
    _goml_runtime_core_string_println(inline273)
    var t255 string
    var inline271 float64 = 0
    switch inline271 {
    case -0:
        t255 = "zero"
    default:
        t255 = "other"
    }
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t255)
    _goml_runtime_core_string_println(inline268)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t264 string = _goml_runtime_core_int32_to_string(self__72)
    return t264
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
