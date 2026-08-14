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
    var inline245 bool = true
    if inline245 {
        var inline246 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline246
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline242 bool = false
    if inline242 {
        var inline243 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline243
    } else {
        none_number__6 = Nothing{}
    }
    var t206 string
    t206 = "Point"
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline238)
    var t207 string
    switch some_number__5.(type) {
    case Just:
        var inline232 int32 = some_number__5.(Just)._0
        var inline234 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline232)
        var inline235 string = "Just(" + inline234
        var inline236 string = inline235 + ")"
        t207 = inline236
    case Nothing:
        t207 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline229)
    var t208 string
    switch none_number__6.(type) {
    case Just:
        var inline223 int32 = none_number__6.(Just)._0
        var inline225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline223)
        var inline226 string = "Just(" + inline225
        var inline227 string = inline226 + ")"
        t208 = inline227
    case Nothing:
        t208 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__33)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
