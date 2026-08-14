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
    var t263 int32
    var inline292 int32 = 12
    t263 = inline292
    var inline288 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t263)
    _goml_runtime_core_string_println(inline288)
    var t264 int32
    var inline286 bool = true
    switch inline286 {
    case true:
        t264 = 1
    case false:
        t264 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline283 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t264)
    _goml_runtime_core_string_println(inline283)
    var t265 string
    var inline281 float64 = 0
    switch inline281 {
    case -0:
        t265 = "zero"
    default:
        t265 = "other"
    }
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline278)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t274 string = _goml_runtime_core_int32_to_string(self__70)
    return t274
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
