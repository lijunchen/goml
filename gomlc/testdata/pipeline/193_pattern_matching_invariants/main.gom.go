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
    var retv223 int32
    var mtmp160 Single = value__5
    var jp225 int32
    switch mtmp160.(type) {
    case Only:
        var x161 int32 = mtmp160.(Only)._0
        var inner__6 int32 = x161
        jp225 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv223 = jp225
    return retv223
}

func decide(flag__7 bool) int32 {
    var retv227 int32
    switch flag__7 {
    case true:
        retv227 = 1
    case false:
        retv227 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv227
}

func signed_zero(value__8 float64) string {
    var retv231 string
    var jp233 string
    switch value__8 {
    case -0:
        jp233 = "zero"
    default:
        jp233 = "other"
    }
    retv231 = jp233
    return retv231
}

func main0() struct{} {
    var t235 Single = Only{
        _0: 12,
    }
    var mtmp162 Tuple1_6Single = Tuple1_6Single{
        _0: t235,
    }
    var x163 Single = mtmp162._0
    var single__9 Single = x163
    var t236 int32 = unwrap(single__9)
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
    var retv246 string
    var t247 string = _goml_runtime_core_int32_to_string(self__43)
    retv246 = t247
    return retv246
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv249 string
    retv249 = self__38
    return retv249
}

func main() {
    main0()
}
