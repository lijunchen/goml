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
    var t154 Box__int = Box__int{
        value: 5,
    }
    var t155 string = _goml_m_describe____B__Box_l_int_r_____T__int(t154)
    _goml_runtime_core_string_println(t155)
    var value__4 Box__int32 = Box__int32{
        value: 6,
    }
    var t156 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__4)
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t156)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int_r_____T__int(value__3 Box__int) string {
    var retv160 string
    var t161 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(value__3)
    var t162 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t161)
    var t163 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(value__3)
    var t164 string = t162 + t163
    var t165 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(value__3)
    var t166 string = t164 + t165
    retv160 = t166
    return retv160
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv168 int32
    var t169 int32 = self__0.value
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv171 string
    var t172 string = _goml_runtime_core_int32_to_string(self__6)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var retv174 int
    var t175 int = self__0.value
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv177 string
    var t178 string = _goml_runtime_core_int_to_string(self__40)
    retv177 = t178
    return retv177
}

func _goml_m_trait__impl_i_Render_i_Box____int_i_render(self__1 Box__int) string {
    var retv180 string
    retv180 = ":render"
    return retv180
}

func _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(self__2 Box__int) string {
    var retv182 string
    retv182 = ":child"
    return retv182
}

func main() {
    main0()
}
