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
    var t268 int32
    var inline297 int32 = 12
    t268 = inline297
    var inline293 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t268)
    _goml_runtime_core_string_println(inline293)
    var t269 int32
    var inline291 bool = true
    switch inline291 {
    case true:
        t269 = 1
    case false:
        t269 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline288 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t269)
    _goml_runtime_core_string_println(inline288)
    var t270 string
    var inline286 float64 = 0
    switch inline286 {
    case -0:
        t270 = "zero"
    default:
        t270 = "other"
    }
    var inline283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline283)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t279 string = _goml_runtime_core_int32_to_string(self__70)
    return t279
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
