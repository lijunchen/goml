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
    var t63 Box__int32 = Box__int32{
        value: 5,
    }
    var t64 string = _goml_m_describe____B__Box_l_int32_r_____T__int32(t63)
    _goml_runtime_core_string_println(t64)
    var t65 Box__int32 = Box__int32{
        value: 6,
    }
    var t66 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int32_r_____T__int32(value__3 Box__int32) string {
    var retv70 string
    var t71 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__3)
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t71)
    var t73 string = _goml_m_trait__impl_i_Render_i_Box____int32_i_render(value__3)
    var t74 string = t72 + t73
    var t75 string = _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(value__3)
    var t76 string = t74 + t75
    retv70 = t76
    return retv70
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv78 int32
    var t79 int32 = self__0.value
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__5)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__41)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_Render_i_Box____int32_i_render(self__1 Box__int32) string {
    var retv87 string
    retv87 = ":render"
    return retv87
}

func _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(self__2 Box__int32) string {
    var retv89 string
    retv89 = ":child"
    return retv89
}

func main() {
    main0()
}
