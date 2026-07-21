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
    var retv129 int32
    var mtmp66 Single = value__5
    var jp131 int32
    switch mtmp66.(type) {
    case Only:
        var x67 int32 = mtmp66.(Only)._0
        var inner__6 int32 = x67
        jp131 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func decide(flag__7 bool) int32 {
    var retv133 int32
    switch flag__7 {
    case true:
        retv133 = 1
    case false:
        retv133 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv133
}

func signed_zero(value__8 float64) string {
    var retv137 string
    var jp139 string
    switch value__8 {
    case -0:
        jp139 = "zero"
    default:
        jp139 = "other"
    }
    retv137 = jp139
    return retv137
}

func main0() struct{} {
    var t141 Single = Only{
        _0: 12,
    }
    var mtmp68 Tuple1_6Single = Tuple1_6Single{
        _0: t141,
    }
    var x69 Single = mtmp68._0
    var single__9 Single = x69
    var t142 int32 = unwrap(single__9)
    println__T_int32(t142)
    var t143 int32 = decide(true)
    println__T_int32(t143)
    var t144 string = signed_zero(0)
    println__T_string(t144)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv152 string
    var t153 string = _goml_runtime_core_int32_to_string(self__41)
    retv152 = t153
    return retv152
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv155 string
    retv155 = self__37
    return retv155
}

func main() {
    main0()
}
