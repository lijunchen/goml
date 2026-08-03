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
    var t179 Box__int = Box__int{
        value: 5,
    }
    var t180 string
    var inline224 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t179)
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline224)
    var inline226 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t179)
    var inline227 string = inline225 + inline226
    var inline228 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t179)
    var inline229 string = inline227 + inline228
    t180 = inline229
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline221)
    var t181 int32
    var inline219 int32 = 6
    t181 = inline219
    var t182 string
    var inline217 string = _goml_runtime_core_int32_to_string(t181)
    t182 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t205 int = self__0.value
    return t205
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t208 string = _goml_runtime_core_int_to_string(self__69)
    return t208
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
