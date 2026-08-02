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
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t159 string = "m" + t158
    return t159
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t162 int32 = self__1.value
    return t162
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t165 int32 = self__2.value
    return t165
}

func main0() struct{} {
    var t167 LeftSource = LeftSource{
        value: 3,
    }
    var t168 RightSource = RightSource{
        value: 4,
    }
    var t169 string = combine__A_LeftSource__B_RightSource(t167, t168)
    println__T_string(t169)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t172 string = _goml_runtime_core_int32_to_string(self__6)
    return t172
}

func println__T_string(value__1 string) struct{} {
    var t174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t174)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var t178 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t179 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t178)
    var t180 string = t179 + ":"
    var t181 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t182 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t181)
    var t183 string = t180 + t182
    return t183
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
