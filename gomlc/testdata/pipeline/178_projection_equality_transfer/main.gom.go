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

type LeftSource struct {
    value int32
}

type RightSource struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_marked(self__0 int32) string {
    var retv70 string
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t72 string = "m" + t71
    retv70 = t72
    return retv70
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv74 int32
    var t75 int32 = self__1.value
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv77 int32
    var t78 int32 = self__2.value
    retv77 = t78
    return retv77
}

func main0() struct{} {
    var t80 LeftSource = LeftSource{
        value: 3,
    }
    var t81 RightSource = RightSource{
        value: 4,
    }
    var t82 string = combine__A_LeftSource__B_RightSource(t80, t81)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__6)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv90 string
    var t91 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t92 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t91)
    var t93 string = t92 + ":"
    var t94 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t95 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t94)
    var t96 string = t93 + t95
    retv90 = t96
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
