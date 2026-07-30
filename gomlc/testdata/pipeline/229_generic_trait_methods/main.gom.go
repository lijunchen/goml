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
    var t112 Prefix = Prefix{
        value: "direct:",
    }
    var direct__3 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(t112, "ok")
    println__T_string(direct__3)
    var t113 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string = render_generic__T_Prefix(t113)
    println__T_string(generic__4)
    var t114 Prefix = Prefix{
        value: "ufcs:",
    }
    var ufcs__5 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t114, 12)
    println__T_string(ufcs__5)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono1(self__0 Prefix, value__1 string) string {
    var retv116 string
    var t117 string = self__0.value
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    var t119 string = t117 + t118
    retv116 = t119
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func render_generic__T_Prefix(renderer__2 Prefix) string {
    var retv124 string
    var t125 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(renderer__2, 11)
    retv124 = t125
    return retv124
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var retv127 string
    var t128 string = self__0.value
    var t129 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    var t130 string = t128 + t129
    retv127 = t130
    return retv127
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv132 string
    retv132 = self__38
    return retv132
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int_to_string(self__40)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
