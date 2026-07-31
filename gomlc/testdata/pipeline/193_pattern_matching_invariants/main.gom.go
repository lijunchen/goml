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
    var retv220 int32
    var mtmp157 Single = value__5
    var jp222 int32
    switch mtmp157.(type) {
    case Only:
        var x158 int32 = mtmp157.(Only)._0
        var inner__6 int32 = x158
        jp222 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv220 = jp222
    return retv220
}

func decide(flag__7 bool) int32 {
    var retv224 int32
    switch flag__7 {
    case true:
        retv224 = 1
    case false:
        retv224 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv224
}

func signed_zero(value__8 float64) string {
    var retv228 string
    var jp230 string
    switch value__8 {
    case -0:
        jp230 = "zero"
    default:
        jp230 = "other"
    }
    retv228 = jp230
    return retv228
}

func main0() struct{} {
    var t232 Single = Only{
        _0: 12,
    }
    var mtmp159 Tuple1_6Single = Tuple1_6Single{
        _0: t232,
    }
    var x160 Single = mtmp159._0
    var single__9 Single = x160
    var t233 int32 = unwrap(single__9)
    println__T_int32(t233)
    var t234 int32 = decide(true)
    println__T_int32(t234)
    var t235 string = signed_zero(0)
    println__T_string(t235)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv243 string
    var t244 string = _goml_runtime_core_int32_to_string(self__43)
    retv243 = t244
    return retv243
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv246 string
    retv246 = self__38
    return retv246
}

func main() {
    main0()
}
