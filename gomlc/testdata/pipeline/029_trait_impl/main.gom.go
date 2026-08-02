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
    var inline213 bool = true
    if inline213 {
        var inline214 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline214
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline210 bool = false
    if inline210 {
        var inline211 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline211
    } else {
        none_number__6 = Nothing{}
    }
    var t174 string
    t174 = "Point"
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline206)
    var t175 string
    switch some_number__5.(type) {
    case Just:
        var inline200 int32 = some_number__5.(Just)._0
        var inline202 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline200)
        var inline203 string = "Just(" + inline202
        var inline204 string = inline203 + ")"
        t175 = inline204
    case Nothing:
        t175 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline197)
    var t176 string
    switch none_number__6.(type) {
    case Just:
        var inline191 int32 = none_number__6.(Just)._0
        var inline193 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline191)
        var inline194 string = "Just(" + inline193
        var inline195 string = inline194 + ")"
        t176 = inline195
    case Nothing:
        t176 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline188)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    return t179
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
