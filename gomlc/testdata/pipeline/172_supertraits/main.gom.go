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
    var t138 Box__int = Box__int{
        value: 5,
    }
    var t139 string
    var inline183 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t138)
    var inline184 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline183)
    var inline185 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t138)
    var inline186 string = inline184 + inline185
    var inline187 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t138)
    var inline188 string = inline186 + inline187
    t139 = inline188
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t139)
    _goml_runtime_core_string_println(inline180)
    var t140 int32
    var inline178 int32 = 6
    t140 = inline178
    var t141 string
    var inline176 string = _goml_runtime_core_int32_to_string(t140)
    t141 = inline176
    var inline173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
    _goml_runtime_core_string_println(inline173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t164 int = self__0.value
    return t164
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t167 string = _goml_runtime_core_int_to_string(self__69)
    return t167
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
