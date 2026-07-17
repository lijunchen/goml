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
    var retv63 string
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t65 string = "m" + t64
    retv63 = t65
    return retv63
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv67 int32
    var t68 int32 = self__1.value
    retv67 = t68
    return retv67
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv70 int32
    var t71 int32 = self__2.value
    retv70 = t71
    return retv70
}

func main0() struct{} {
    var t73 LeftSource = LeftSource{
        value: 3,
    }
    var t74 RightSource = RightSource{
        value: 4,
    }
    var t75 string = combine__A_LeftSource__B_RightSource(t73, t74)
    println__T_string(t75)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv77 string
    var t78 string = _goml_runtime_core_int32_to_string(self__5)
    retv77 = t78
    return retv77
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv83 string
    var t84 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t85 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t84)
    var t86 string = t85 + ":"
    var t87 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t88 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t87)
    var t89 string = t86 + t88
    retv83 = t89
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv91 string
    retv91 = self__37
    return retv91
}

func main() {
    main0()
}
