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
    var t110 Box__int = Box__int{
        value: 5,
    }
    var t111 string = _goml_m_describe____B__Box_l_int_r_____T__int(t110)
    _goml_runtime_core_string_println(t111)
    var value__4 Box__int32 = Box__int32{
        value: 6,
    }
    var t112 int32 = _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(value__4)
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t112)
    _goml_runtime_core_string_println(t113)
    return struct{}{}
}

func _goml_m_describe____B__Box_l_int_r_____T__int(value__3 Box__int) string {
    var retv116 string
    var t117 int = _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(value__3)
    var t118 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t117)
    var t119 string = _goml_m_trait__impl_i_Render_i_Box____int_i_render(value__3)
    var t120 string = t118 + t119
    var t121 string = _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(value__3)
    var t122 string = t120 + t121
    retv116 = t122
    return retv116
}

func _goml_m_trait__impl_i_Parent_i__l_int32_r__x40_Box____int32_i_parent(self__0 Box__int32) int32 {
    var retv124 int32
    var t125 int32 = self__0.value
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv127 string
    var t128 string = _goml_runtime_core_int32_to_string(self__6)
    retv127 = t128
    return retv127
}

func _goml_m_trait__impl_i_Parent_i__l_int_r__x40_Box____int_i_parent(self__0 Box__int) int {
    var retv130 int
    var t131 int = self__0.value
    retv130 = t131
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv133 string
    var t134 string = _goml_runtime_core_int_to_string(self__40)
    retv133 = t134
    return retv133
}

func _goml_m_trait__impl_i_Render_i_Box____int_i_render(self__1 Box__int) string {
    var retv136 string
    retv136 = ":render"
    return retv136
}

func _goml_m_trait__impl_i_Child_i__l_int_r__x40_Box____int_i_child(self__2 Box__int) string {
    var retv138 string
    retv138 = ":child"
    return retv138
}

func main() {
    main0()
}
