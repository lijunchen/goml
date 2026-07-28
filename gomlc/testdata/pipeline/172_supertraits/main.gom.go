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
    var t66 Box__int = Box__int{
        value: 5,
    }
    var t67 string = _goml_m_describe____B__Box_l_int_r_____T__int(t66)
    _goml_runtime_core_string_println(t67)
    var value__4 Box__int32 = Box__int32{
        value: 6,
    }
    var t68 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__4)
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t68)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int_r_____T__int(value__3 Box__int) string {
    var retv72 string
    var t73 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(value__3)
    var t74 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t73)
    var t75 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(value__3)
    var t76 string = t74 + t75
    var t77 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(value__3)
    var t78 string = t76 + t77
    retv72 = t78
    return retv72
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv80 int32
    var t81 int32 = self__0.value
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv83 string
    var t84 string = _goml_runtime_core_int32_to_string(self__6)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var retv86 int
    var t87 int = self__0.value
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int_to_string(self__40)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_Render_i_Box____int_i_render(self__1 Box__int) string {
    var retv92 string
    retv92 = ":render"
    return retv92
}

func _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(self__2 Box__int) string {
    var retv94 string
    retv94 = ":child"
    return retv94
}

func main() {
    main0()
}
