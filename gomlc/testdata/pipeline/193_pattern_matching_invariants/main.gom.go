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

func unwrap(value__5 Single) int32 {
    switch value__5.(type) {
    case Only:
        var x161 int32 = value__5.(Only)._0
        return x161
    default:
        panic("non-exhaustive match")
    }
}

func decide(flag__7 bool) int32 {
    switch flag__7 {
    case true:
        return 1
    case false:
        return 2
    default:
        panic("non-exhaustive match")
    }
}

func signed_zero(value__8 float64) string {
    switch value__8 {
    case -0:
        return "zero"
    default:
        return "other"
    }
}

func main0() struct{} {
    var t235 Single = Only{
        _0: 12,
    }
    var t236 int32 = unwrap(t235)
    println__T_int32(t236)
    var t237 int32 = decide(true)
    println__T_int32(t237)
    var t238 string = signed_zero(0)
    println__T_string(t238)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t243)
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
