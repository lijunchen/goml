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
    var retv154 string
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t156 string = "m" + t155
    retv154 = t156
    return retv154
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv158 int32
    var t159 int32 = self__1.value
    retv158 = t159
    return retv158
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv161 int32
    var t162 int32 = self__2.value
    retv161 = t162
    return retv161
}

func main0() struct{} {
    var t164 LeftSource = LeftSource{
        value: 3,
    }
    var t165 RightSource = RightSource{
        value: 4,
    }
    var t166 string = combine__A_LeftSource__B_RightSource(t164, t165)
    println__T_string(t166)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv168 string
    var t169 string = _goml_runtime_core_int32_to_string(self__6)
    retv168 = t169
    return retv168
}

func println__T_string(value__1 string) struct{} {
    var t171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv174 string
    var t175 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t176 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t175)
    var t177 string = t176 + ":"
    var t178 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t179 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t178)
    var t180 string = t177 + t179
    retv174 = t180
    return retv174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv182 string
    retv182 = self__38
    return retv182
}

func main() {
    main0()
}
