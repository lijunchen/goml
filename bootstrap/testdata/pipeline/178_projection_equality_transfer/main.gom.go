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
    var retv66 string
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t68 string = "m" + t67
    retv66 = t68
    return retv66
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv70 int32
    var t71 int32 = self__1.value
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv73 int32
    var t74 int32 = self__2.value
    retv73 = t74
    return retv73
}

func main0() struct{} {
    var t76 LeftSource = LeftSource{
        value: 3,
    }
    var t77 RightSource = RightSource{
        value: 4,
    }
    var t78 string = combine__A_LeftSource__B_RightSource(t76, t77)
    println__T_string(t78)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int32_to_string(self__6)
    retv80 = t81
    return retv80
}

func println__T_string(value__1 string) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t83)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv86 string
    var t87 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t88 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t87)
    var t89 string = t88 + ":"
    var t90 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t91 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t90)
    var t92 string = t89 + t91
    retv86 = t92
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv94 string
    retv94 = self__38
    return retv94
}

func main() {
    main0()
}
