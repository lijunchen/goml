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
    var retv60 string
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t62 string = "m" + t61
    retv60 = t62
    return retv60
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv64 int32
    var t65 int32 = self__1.value
    retv64 = t65
    return retv64
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv67 int32
    var t68 int32 = self__2.value
    retv67 = t68
    return retv67
}

func main0() struct{} {
    var t70 LeftSource = LeftSource{
        value: 3,
    }
    var t71 RightSource = RightSource{
        value: 4,
    }
    var t72 string = combine__A_LeftSource__B_RightSource(t70, t71)
    println__T_string(t72)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__2)
    retv74 = t75
    return retv74
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv80 string
    var t81 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t82 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t81)
    var t83 string = t82 + ":"
    var t84 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t85 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t84)
    var t86 string = t83 + t85
    retv80 = t86
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv88 string
    retv88 = self__34
    return retv88
}

func main() {
    main0()
}
