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
    var t156 Prefix = Prefix{
        value: "direct:",
    }
    var direct__3 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(t156, "ok")
    println__T_string(direct__3)
    var t157 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string = render_generic__T_Prefix(t157)
    println__T_string(generic__4)
    var t158 Prefix = Prefix{
        value: "ufcs:",
    }
    var ufcs__5 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t158, 12)
    println__T_string(ufcs__5)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(self__0 Prefix, value__1 string) string {
    var retv160 string
    var t161 string = self__0.value
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    var t163 string = t161 + t162
    retv160 = t163
    return retv160
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func render_generic__T_Prefix(renderer__2 Prefix) string {
    var retv168 string
    var t169 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(renderer__2, 11)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var retv171 string
    var t172 string = self__0.value
    var t173 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    var t174 string = t172 + t173
    retv171 = t174
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv176 string
    retv176 = self__38
    return retv176
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int_to_string(self__40)
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
