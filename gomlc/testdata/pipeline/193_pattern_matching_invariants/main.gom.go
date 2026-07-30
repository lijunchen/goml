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
    var retv136 int32
    var mtmp73 Single = value__5
    var jp138 int32
    switch mtmp73.(type) {
    case Only:
        var x74 int32 = mtmp73.(Only)._0
        var inner__6 int32 = x74
        jp138 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv136 = jp138
    return retv136
}

func decide(flag__7 bool) int32 {
    var retv140 int32
    switch flag__7 {
    case true:
        retv140 = 1
    case false:
        retv140 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv140
}

func signed_zero(value__8 float64) string {
    var retv144 string
    var jp146 string
    switch value__8 {
    case -0:
        jp146 = "zero"
    default:
        jp146 = "other"
    }
    retv144 = jp146
    return retv144
}

func main0() struct{} {
    var t148 Single = Only{
        _0: 12,
    }
    var mtmp75 Tuple1_6Single = Tuple1_6Single{
        _0: t148,
    }
    var x76 Single = mtmp75._0
    var single__9 Single = x76
    var t149 int32 = unwrap(single__9)
    println__T_int32(t149)
    var t150 int32 = decide(true)
    println__T_int32(t150)
    var t151 string = signed_zero(0)
    println__T_string(t151)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t153 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t153)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv159 string
    var t160 string = _goml_runtime_core_int32_to_string(self__43)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv162 string
    retv162 = self__38
    return retv162
}

func main() {
    main0()
}
