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
    var retv24 string
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t26 string = "m" + t25
    retv24 = t26
    return retv24
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv28 int32
    var t29 int32 = self__1.value
    retv28 = t29
    return retv28
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv31 int32
    var t32 int32 = self__2.value
    retv31 = t32
    return retv31
}

func main0() struct{} {
    var t34 LeftSource = LeftSource{
        value: 3,
    }
    var t35 RightSource = RightSource{
        value: 4,
    }
    var t36 string = combine__A_LeftSource__B_RightSource(t34, t35)
    println__T_string(t36)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int32_to_string(self__2)
    retv38 = t39
    return retv38
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv44 string
    var t45 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t46 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t45)
    var t47 string = t46 + ":"
    var t48 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t49 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t48)
    var t50 string = t47 + t49
    retv44 = t50
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv52 string
    retv52 = self__9
    return retv52
}

func main() {
    main0()
}
