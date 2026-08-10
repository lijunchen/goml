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
    var inline230 bool = true
    if inline230 {
        var inline231 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline231
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline227 bool = false
    if inline227 {
        var inline228 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline228
    } else {
        none_number__6 = Nothing{}
    }
    var t191 string
    t191 = "Point"
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline223)
    var t192 string
    switch some_number__5.(type) {
    case Just:
        var inline217 int32 = some_number__5.(Just)._0
        var inline219 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline217)
        var inline220 string = "Just(" + inline219
        var inline221 string = inline220 + ")"
        t192 = inline221
    case Nothing:
        t192 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline214)
    var t193 string
    switch none_number__6.(type) {
    case Just:
        var inline208 int32 = none_number__6.(Just)._0
        var inline210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline208)
        var inline211 string = "Just(" + inline210
        var inline212 string = inline211 + ")"
        t193 = inline212
    case Nothing:
        t193 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t196 string = _goml_runtime_core_int32_to_string(self__33)
    return t196
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
