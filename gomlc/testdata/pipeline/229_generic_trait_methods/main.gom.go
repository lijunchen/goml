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
    var t159 Prefix = Prefix{
        value: "direct:",
    }
    var direct__3 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(t159, "ok")
    println__T_string(direct__3)
    var t160 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string = render_generic__T_Prefix(t160)
    println__T_string(generic__4)
    var t161 Prefix = Prefix{
        value: "ufcs:",
    }
    var ufcs__5 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t161, 12)
    println__T_string(ufcs__5)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(self__0 Prefix, value__1 string) string {
    var t164 string = self__0.value
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    var t166 string = t164 + t165
    return t166
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func render_generic__T_Prefix(renderer__2 Prefix) string {
    var t172 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(renderer__2, 11)
    return t172
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t175 string = self__0.value
    var t176 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
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
