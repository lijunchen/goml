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
    var t492 int32
    var inline521 int32 = 12
    t492 = inline521
    var inline517 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t492)
    _goml_runtime_core_string_println(inline517)
    var t493 int32
    var inline515 bool = true
    switch inline515 {
    case true:
        t493 = 1
    case false:
        t493 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline512 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t493)
    _goml_runtime_core_string_println(inline512)
    var t494 string
    var inline510 float64 = 0
    switch inline510 {
    case -0:
        t494 = "zero"
    default:
        t494 = "other"
    }
    var inline507 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t494)
    _goml_runtime_core_string_println(inline507)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t503 string = _goml_runtime_core_int32_to_string(self__154)
    return t503
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
