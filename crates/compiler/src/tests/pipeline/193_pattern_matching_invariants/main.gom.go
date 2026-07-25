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
    var retv132 int32
    var mtmp69 Single = value__5
    var jp134 int32
    switch mtmp69.(type) {
    case Only:
        var x70 int32 = mtmp69.(Only)._0
        var inner__6 int32 = x70
        jp134 = inner__6
    default:
        panic("non-exhaustive match")
    }
    retv132 = jp134
    return retv132
}

func decide(flag__7 bool) int32 {
    var retv136 int32
    switch flag__7 {
    case true:
        retv136 = 1
    case false:
        retv136 = 2
    default:
        panic("non-exhaustive match")
    }
    return retv136
}

func signed_zero(value__8 float64) string {
    var retv140 string
    var jp142 string
    switch value__8 {
    case -0:
        jp142 = "zero"
    default:
        jp142 = "other"
    }
    retv140 = jp142
    return retv140
}

func main0() struct{} {
    var t144 Single = Only{
        _0: 12,
    }
    var mtmp71 Tuple1_6Single = Tuple1_6Single{
        _0: t144,
    }
    var x72 Single = mtmp71._0
    var single__9 Single = x72
    var t145 int32 = unwrap(single__9)
    println__T_int32(t145)
    var t146 int32 = decide(true)
    println__T_int32(t146)
    var t147 string = signed_zero(0)
    println__T_string(t147)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t152 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t152)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv155 string
    var t156 string = _goml_runtime_core_int32_to_string(self__43)
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv158 string
    retv158 = self__38
    return retv158
}

func main() {
    main0()
}
