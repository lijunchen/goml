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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var t192 int32 = self__0.value
    return t192
}

func main0() struct{} {
    var t194 Number = Number{
        value: 42,
    }
    var t195 int32
    var inline250 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t194)
    t195 = inline250
    var t196 string
    var inline248 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline245)
    var t197 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline243 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t197)
    value__4 = inline243
    var t198 string
    var inline241 string = _goml_runtime_core_int32_to_string(value__4)
    t198 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline238)
    var t199 Box__string = Box__string{
        value: "generic",
    }
    var t200 string
    var inline236 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t199)
    t200 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline233)
    var t202 int32
    var inline231 int32 = 11
    t202 = inline231
    var t203 string
    var inline229 string = _goml_runtime_core_int32_to_string(t202)
    t203 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t224 string = self__1.value
    return t224
}

func main() {
    main0()
}
