package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Box__int32 struct {
    value int32
}

func main0() struct{} {
    var t60 Box__int32 = Box__int32{
        value: 5,
    }
    var t61 string = _goml_m_describe____B__Box_l_int32_r_____T__int32(t60)
    _goml_runtime_core_string_println(t61)
    var t62 Box__int32 = Box__int32{
        value: 6,
    }
    var t63 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(t62)
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t63)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int32_r_____T__int32(value__3 Box__int32) string {
    var retv67 string
    var t68 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__3)
    var t69 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t68)
    var t70 string = _goml_m_trait__impl_i_Render_i_Box____int32_i_render(value__3)
    var t71 string = t69 + t70
    var t72 string = _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(value__3)
    var t73 string = t71 + t72
    retv67 = t73
    return retv67
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv75 int32
    var t76 int32 = self__0.value
    retv75 = t76
    return retv75
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__2)
    retv78 = t79
    return retv78
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__38)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_Render_i_Box____int32_i_render(self__1 Box__int32) string {
    var retv84 string
    retv84 = ":render"
    return retv84
}

func _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(self__2 Box__int32) string {
    var retv86 string
    retv86 = ":child"
    return retv86
}

func main() {
    main0()
}
