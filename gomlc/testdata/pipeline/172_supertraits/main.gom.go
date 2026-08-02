package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Box__int struct {
    value int
}

type Box__int32 struct {
    value int32
}

func main0() struct{} {
    var t157 Box__int = Box__int{
        value: 5,
    }
    var t158 string
    var inline202 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t157)
    var inline203 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline202)
    var inline204 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t157)
    var inline205 string = inline203 + inline204
    var inline206 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t157)
    var inline207 string = inline205 + inline206
    t158 = inline207
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline199)
    var t159 int32
    var inline197 int32 = 6
    t159 = inline197
    var t160 string
    var inline195 string = _goml_runtime_core_int32_to_string(t159)
    t160 = inline195
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline192)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t183 int = self__0.value
    return t183
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t186 string = _goml_runtime_core_int_to_string(self__40)
    return t186
}

func _goml_m_trait__impl_i_Render_i_Box____int_i_render(self__1 Box__int) string {
    return ":render"
}

func _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(self__2 Box__int) string {
    return ":child"
}

func main() {
    main0()
}
