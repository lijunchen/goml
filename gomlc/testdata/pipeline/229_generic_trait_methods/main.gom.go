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
    var inline181 string = "ok"
    var inline182 string = "direct:"
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline181)
    var inline184 string = inline182 + inline183
    direct__3 = inline184
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline178)
    var t141 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline176 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t141, 11)
    generic__4 = inline176
    var inline173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline173)
    var ufcs__5 string
    var inline168 int = 12
    var inline169 string = "ufcs:"
    var inline170 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline168)
    var inline171 string = inline169 + inline170
    ufcs__5 = inline171
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t156 string = self__0.value
    var t157 string
    var inline193 string = _goml_runtime_core_int_to_string(value__1)
    t157 = inline193
    var t158 string = t156 + t157
    return t158
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t163 string = _goml_runtime_core_int_to_string(self__69)
    return t163
}

func main() {
    main0()
}
