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
    var inline222 string = "ok"
    var inline223 string = "direct:"
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline222)
    var inline225 string = inline223 + inline224
    direct__3 = inline225
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline219)
    var t182 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline217 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t182, 11)
    generic__4 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline214)
    var ufcs__5 string
    var inline209 int = 12
    var inline210 string = "ufcs:"
    var inline211 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline209)
    var inline212 string = inline210 + inline211
    ufcs__5 = inline212
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t197 string = self__0.value
    var t198 string
    var inline234 string = _goml_runtime_core_int_to_string(value__1)
    t198 = inline234
    var t199 string = t197 + t198
    return t199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t204 string = _goml_runtime_core_int_to_string(self__69)
    return t204
}

func main() {
    main0()
}
