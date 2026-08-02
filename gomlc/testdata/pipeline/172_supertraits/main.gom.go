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
    var inline191 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t157)
    var inline192 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline191)
    var inline193 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t157)
    var inline194 string = inline192 + inline193
    var inline195 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t157)
    var inline196 string = inline194 + inline195
    t158 = inline196
    _goml_runtime_core_string_println(t158)
    var t159 int32
    var inline189 int32 = 6
    t159 = inline189
    var t160 string
    var inline187 string = _goml_runtime_core_int32_to_string(t159)
    t160 = inline187
    _goml_runtime_core_string_println(t160)
    return struct{}{}
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
