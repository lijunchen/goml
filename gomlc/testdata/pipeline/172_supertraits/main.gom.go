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
    var t189 Box__int = Box__int{
        value: 5,
    }
    var t190 string
    var inline234 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t189)
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline234)
    var inline236 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t189)
    var inline237 string = inline235 + inline236
    var inline238 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t189)
    var inline239 string = inline237 + inline238
    t190 = inline239
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline231)
    var t191 int32
    var inline229 int32 = 6
    t191 = inline229
    var t192 string
    var inline227 string = _goml_runtime_core_int32_to_string(t191)
    t192 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline224)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t215 int = self__0.value
    return t215
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t218 string = _goml_runtime_core_int_to_string(self__67)
    return t218
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
