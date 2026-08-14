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
    var t184 Box__int = Box__int{
        value: 5,
    }
    var t185 string
    var inline229 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t184)
    var inline230 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline229)
    var inline231 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t184)
    var inline232 string = inline230 + inline231
    var inline233 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t184)
    var inline234 string = inline232 + inline233
    t185 = inline234
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline226)
    var t186 int32
    var inline224 int32 = 6
    t186 = inline224
    var t187 string
    var inline222 string = _goml_runtime_core_int32_to_string(t186)
    t187 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t210 int = self__0.value
    return t210
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t213 string = _goml_runtime_core_int_to_string(self__67)
    return t213
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
