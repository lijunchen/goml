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
    var t24 Box__int32 = Box__int32{
        value: 5,
    }
    var t25 string = _goml_m_describe____B__Box_l_int32_r_____T__int32(t24)
    _goml_runtime_core_string_println(t25)
    var t26 Box__int32 = Box__int32{
        value: 6,
    }
    var t27 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(t26)
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t27)
    _goml_runtime_core_string_println(t28)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int32_r_____T__int32(value__3 Box__int32) string {
    var retv31 string
    var t32 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__3)
    var t33 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t32)
    var t34 string = _goml_m_trait__impl_i_Render_i_Box____int32_i_render(value__3)
    var t35 string = t33 + t34
    var t36 string = _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(value__3)
    var t37 string = t35 + t36
    retv31 = t37
    return retv31
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv39 int32
    var t40 int32 = self__0.value
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int32_to_string(self__2)
    retv42 = t43
    return retv42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv45 string
    var t46 string = _goml_runtime_core_int32_to_string(self__13)
    retv45 = t46
    return retv45
}

func _goml_m_trait__impl_i_Render_i_Box____int32_i_render(self__1 Box__int32) string {
    var retv48 string
    retv48 = ":render"
    return retv48
}

func _goml_m_trait__impl_i_Child_i__l_int32_r__x40_Box____int32_i_child(self__2 Box__int32) string {
    var retv50 string
    retv50 = ":child"
    return retv50
}

func main() {
    main0()
}
