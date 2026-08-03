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

type Point struct {
    x int32
    y int32
}

type Maybe__int32 interface {
    isMaybe__int32()
}

type Just struct {
    _0 int32
}

func (_ Just) isMaybe__int32() {}

type Nothing struct {}

func (_ Nothing) isMaybe__int32() {}

func main0() struct{} {
    var some_number__5 Maybe__int32
    var inline235 bool = true
    if inline235 {
        var inline236 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline236
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline232 bool = false
    if inline232 {
        var inline233 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline233
    } else {
        none_number__6 = Nothing{}
    }
    var t196 string
    t196 = "Point"
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline228)
    var t197 string
    switch some_number__5.(type) {
    case Just:
        var inline222 int32 = some_number__5.(Just)._0
        var inline224 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline222)
        var inline225 string = "Just(" + inline224
        var inline226 string = inline225 + ")"
        t197 = inline226
    case Nothing:
        t197 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline219)
    var t198 string
    switch none_number__6.(type) {
    case Just:
        var inline213 int32 = none_number__6.(Just)._0
        var inline215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline213)
        var inline216 string = "Just(" + inline215
        var inline217 string = inline216 + ")"
        t198 = inline217
    case Nothing:
        t198 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t201 string = _goml_runtime_core_int32_to_string(self__35)
    return t201
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
