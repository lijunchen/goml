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
    var retv176 int32
    var mtmp113 Single = value__5
    var jp178 int32
    switch mtmp113.(type) {
    case Only:
        var x114 int32 = mtmp113.(Only)._0
        var inner__6 int32 = x114
        jp178 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv176 = jp178
    return retv176
}

func decide(flag__7 bool) int32 {
    var retv180 int32
    switch flag__7 {
    case true:
        retv180 = 1
    case false:
        retv180 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv180
}

func signed_zero(value__8 float64) string {
    var retv184 string
    var jp186 string
    switch value__8 {
    case -0:
        jp186 = "zero"
    default:
        jp186 = "other"
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var t188 Single = Only{
        _0: 12,
    }
    var mtmp115 Tuple1_6Single = Tuple1_6Single{
        _0: t188,
    }
    var x116 Single = mtmp115._0
    var single__9 Single = x116
    var t189 int32 = unwrap(single__9)
    println__T_int32(t189)
    var t190 int32 = decide(true)
    println__T_int32(t190)
    var t191 string = signed_zero(0)
    println__T_string(t191)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv199 string
    var t200 string = _goml_runtime_core_int32_to_string(self__43)
    retv199 = t200
    return retv199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv202 string
    retv202 = self__38
    return retv202
}

func main() {
    main0()
}
