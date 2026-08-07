package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Prefix struct {
    value string
}

func main0() struct{} {
    var direct__3 string
    var inline217 string = "ok"
    var inline218 string = "direct:"
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline217)
    var inline220 string = inline218 + inline219
    direct__3 = inline220
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline214)
    var t177 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline212 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t177, 11)
    generic__4 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline209)
    var ufcs__5 string
    var inline204 int = 12
    var inline205 string = "ufcs:"
    var inline206 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline204)
    var inline207 string = inline205 + inline206
    ufcs__5 = inline207
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t192 string = self__0.value
    var t193 string
    var inline229 string = _goml_runtime_core_int_to_string(value__1)
    t193 = inline229
    var t194 string = t192 + t193
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t199 string = _goml_runtime_core_int_to_string(self__69)
    return t199
}

func main() {
    main0()
}
