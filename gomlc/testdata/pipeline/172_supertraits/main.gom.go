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

type Box__isize struct {
    value int
}

type Box__i32 struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t413 Box__isize = Box__isize{
        value: 5,
    }
    var t414 string
    var inline458 int = _goml_m_trait__impl_i_Parent_i__l_isize_r__x40_Box____isize_i_parent(t413)
    var inline459 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline458)
    var inline460 string = _goml_m_trait__impl_i_Render_i_Box____isize_i_render(t413)
    var inline461 string = inline459 + inline460
    var inline462 string = _goml_m_trait__impl_i_Child_i__l_isize_r__x40_Box____isize_i_child(t413)
    var inline463 string = inline461 + inline462
    t414 = inline463
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline455)
    var t415 int32
    var inline453 int32 = 6
    t415 = inline453
    var t416 string
    var inline451 string = _goml_runtime_core_int32_to_string(t415)
    t416 = inline451
    var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline448)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Parent_i__l_isize_r__x40_Box____isize_i_parent(self__0 Box__isize) int {
    var t439 int = self__0.value
    return t439
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t442 string = _goml_runtime_core_int_to_string(self__151)
    return t442
}

func _goml_m_trait__impl_i_Render_i_Box____isize_i_render(self__1 Box__isize) string {
    return ":render"
}

func _goml_m_trait__impl_i_Child_i__l_isize_r__x40_Box____isize_i_child(self__2 Box__isize) string {
    return ":child"
}

func main() {
    main0()
}
