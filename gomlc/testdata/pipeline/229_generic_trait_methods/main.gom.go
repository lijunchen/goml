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
    var inline232 string = "ok"
    var inline233 string = "direct:"
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
    var inline235 string = inline233 + inline234
    direct__3 = inline235
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline229)
    var t192 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline227 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t192, 11)
    generic__4 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline224)
    var ufcs__5 string
    var inline219 int = 12
    var inline220 string = "ufcs:"
    var inline221 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline219)
    var inline222 string = inline220 + inline221
    ufcs__5 = inline222
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t207 string = self__0.value
    var t208 string
    var inline244 string = _goml_runtime_core_int_to_string(value__1)
    t208 = inline244
    var t209 string = t207 + t208
    return t209
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t214 string = _goml_runtime_core_int_to_string(self__67)
    return t214
}

func main() {
    main0()
}
