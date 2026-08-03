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
    var inline194 bool = true
    if inline194 {
        var inline195 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline195
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline191 bool = false
    if inline191 {
        var inline192 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline192
    } else {
        none_number__6 = Nothing{}
    }
    var t155 string
    t155 = "Point"
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline187)
    var t156 string
    switch some_number__5.(type) {
    case Just:
        var inline181 int32 = some_number__5.(Just)._0
        var inline183 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline181)
        var inline184 string = "Just(" + inline183
        var inline185 string = inline184 + ")"
        t156 = inline185
    case Nothing:
        t156 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
    _goml_runtime_core_string_println(inline178)
    var t157 string
    switch none_number__6.(type) {
    case Just:
        var inline172 int32 = none_number__6.(Just)._0
        var inline174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline172)
        var inline175 string = "Just(" + inline174
        var inline176 string = inline175 + ")"
        t157 = inline176
    case Nothing:
        t157 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline169)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t160 string = _goml_runtime_core_int32_to_string(self__35)
    return t160
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
