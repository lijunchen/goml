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
    var t158 string = _goml_m_describe____B__Box_l_int_r_____T__int(t157)
    _goml_runtime_core_string_println(t158)
    var value__4 Box__int32 = Box__int32{
        value: 6,
    }
    var t159 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__4)
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t159)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int_r_____T__int(value__3 Box__int) string {
    var t164 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(value__3)
    var t165 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t164)
    var t166 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(value__3)
    var t167 string = t165 + t166
    var t168 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(value__3)
    var t169 string = t167 + t168
    return t169
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var t172 int32 = self__0.value
    return t172
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t175 string = _goml_runtime_core_int32_to_string(self__6)
    return t175
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t178 int = self__0.value
    return t178
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t181 string = _goml_runtime_core_int_to_string(self__40)
    return t181
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
