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
    var inline240 bool = true
    if inline240 {
        var inline241 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline241
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline237 bool = false
    if inline237 {
        var inline238 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline238
    } else {
        none_number__6 = Nothing{}
    }
    var t201 string
    t201 = "Point"
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline233)
    var t202 string
    switch some_number__5.(type) {
    case Just:
        var inline227 int32 = some_number__5.(Just)._0
        var inline229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline227)
        var inline230 string = "Just(" + inline229
        var inline231 string = inline230 + ")"
        t202 = inline231
    case Nothing:
        t202 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline224)
    var t203 string
    switch none_number__6.(type) {
    case Just:
        var inline218 int32 = none_number__6.(Just)._0
        var inline220 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline218)
        var inline221 string = "Just(" + inline220
        var inline222 string = inline221 + ")"
        t203 = inline222
    case Nothing:
        t203 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t206 string = _goml_runtime_core_int32_to_string(self__33)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
