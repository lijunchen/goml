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
    var inline200 string = "ok"
    var inline201 string = "direct:"
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline200)
    var inline203 string = inline201 + inline202
    direct__3 = inline203
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline197)
    var t160 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline195 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t160, 11)
    generic__4 = inline195
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline192)
    var ufcs__5 string
    var inline187 int = 12
    var inline188 string = "ufcs:"
    var inline189 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline187)
    var inline190 string = inline188 + inline189
    ufcs__5 = inline190
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t175 string = self__0.value
    var t176 string
    var inline212 string = _goml_runtime_core_int_to_string(value__1)
    t176 = inline212
    var t177 string = t175 + t176
    return t177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t182 string = _goml_runtime_core_int_to_string(self__40)
    return t182
}

func main() {
    main0()
}
