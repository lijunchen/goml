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

type Ordering int32

func main0() struct{} {
    var t410 Box__int = Box__int{
        value: 5,
    }
    var t411 string
    var inline455 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(t410)
    var inline456 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline455)
    var inline457 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(t410)
    var inline458 string = inline456 + inline457
    var inline459 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(t410)
    var inline460 string = inline458 + inline459
    t411 = inline460
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t411)
    _goml_runtime_core_string_println(inline452)
    var t412 int32
    var inline450 int32 = 6
    t412 = inline450
    var t413 string
    var inline448 string = _goml_runtime_core_int32_to_string(t412)
    t413 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var t436 int = self__0.value
    return t436
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t439 string = _goml_runtime_core_int_to_string(self__151)
    return t439
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
