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
    var t70 Box__int = Box__int{
        value: 5,
    }
    var t71 string = _goml_m_describe____B__Box_l_int_r_____T__int(t70)
    _goml_runtime_core_string_println(t71)
    var value__4 Box__int32 = Box__int32{
        value: 6,
    }
    var t72 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__4)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int_r_____T__int(value__3 Box__int) string {
    var retv76 string
    var t77 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(value__3)
    var t78 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t77)
    var t79 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(value__3)
    var t80 string = t78 + t79
    var t81 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(value__3)
    var t82 string = t80 + t81
    retv76 = t82
    return retv76
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv84 int32
    var t85 int32 = self__0.value
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__6)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var retv90 int
    var t91 int = self__0.value
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int_to_string(self__40)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_Render_i_Box____int_i_render(self__1 Box__int) string {
    var retv96 string
    retv96 = ":render"
    return retv96
}

func _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(self__2 Box__int) string {
    var retv98 string
    retv98 = ":child"
    return retv98
}

func main() {
    main0()
}
