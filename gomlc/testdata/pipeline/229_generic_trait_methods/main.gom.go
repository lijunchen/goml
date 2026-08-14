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
    var inline227 string = "ok"
    var inline228 string = "direct:"
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline227)
    var inline230 string = inline228 + inline229
    direct__3 = inline230
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline224)
    var t187 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline222 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t187, 11)
    generic__4 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline219)
    var ufcs__5 string
    var inline214 int = 12
    var inline215 string = "ufcs:"
    var inline216 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline214)
    var inline217 string = inline215 + inline216
    ufcs__5 = inline217
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t202 string = self__0.value
    var t203 string
    var inline239 string = _goml_runtime_core_int_to_string(value__1)
    t203 = inline239
    var t204 string = t202 + t203
    return t204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t209 string = _goml_runtime_core_int_to_string(self__67)
    return t209
}

func main() {
    main0()
}
