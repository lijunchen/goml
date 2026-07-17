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
    var retv130 int32
    var mtmp66 Single = value__5
    var jp132 int32
    switch mtmp66.(type) {
    case Only:
        var x67 int32 = mtmp66.(Only)._0
        var inner__6 int32 = x67
        jp132 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv130 = jp132
    return retv130
}

func decide(flag__7 bool) int32 {
    var retv134 int32
    switch flag__7 {
    case true:
        retv134 = 1
    case false:
        retv134 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv134
}

func signed_zero(value__8 float64) string {
    var retv138 string
    var jp140 string
    switch value__8 {
    case -0:
        jp140 = "zero"
    default:
        jp140 = "other"
    }
    retv138 = jp140
    return retv138
}

func main0() struct{} {
    var t142 Single = Only{
        _0: 12,
    }
    var mtmp68 Tuple1_6Single = Tuple1_6Single{
        _0: t142,
    }
    var x69 Single = mtmp68._0
    var single__9 Single = x69
    var t143 int32 = unwrap(single__9)
    println__T_int32(t143)
    var t144 int32 = decide(true)
    println__T_int32(t144)
    var t145 string = signed_zero(0)
    println__T_string(t145)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t147 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t147)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv153 string
    var t154 string = _goml_runtime_core_int32_to_string(self__41)
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv156 string
    retv156 = self__37
    return retv156
}

func main() {
    main0()
}
