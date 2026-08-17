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

type Ordering int32

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

type Single struct {
    _tag int32
    _v0_0 int32
}

func main0() struct{} {
    var t489 int32
    var inline518 int32 = 12
    t489 = inline518
    var inline514 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t489)
    _goml_runtime_core_string_println(inline514)
    var t490 int32
    var inline512 bool = true
    switch inline512 {
    case true:
        t490 = 1
    case false:
        t490 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline509 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t490)
    _goml_runtime_core_string_println(inline509)
    var t491 string
    var inline507 float64 = 0
    switch inline507 {
    case -0:
        t491 = "zero"
    default:
        t491 = "other"
    }
    var inline504 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t491)
    _goml_runtime_core_string_println(inline504)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t500 string = _goml_runtime_core_int32_to_string(self__154)
    return t500
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
