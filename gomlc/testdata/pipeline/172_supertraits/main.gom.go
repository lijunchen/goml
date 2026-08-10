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
    var t174 Box__int = Box__int{
        value: 5,
    }
    var t175 string
    var inline219 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t174)
    var inline220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline219)
    var inline221 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t174)
    var inline222 string = inline220 + inline221
    var inline223 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t174)
    var inline224 string = inline222 + inline223
    t175 = inline224
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline216)
    var t176 int32
    var inline214 int32 = 6
    t176 = inline214
    var t177 string
    var inline212 string = _goml_runtime_core_int32_to_string(t176)
    t177 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t200 int = self__0.value
    return t200
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t203 string = _goml_runtime_core_int_to_string(self__67)
    return t203
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
