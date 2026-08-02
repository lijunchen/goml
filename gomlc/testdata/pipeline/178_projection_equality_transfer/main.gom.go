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
    var retv157 string
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t159 string = "m" + t158
    retv157 = t159
    return retv157
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var retv161 int32
    var t162 int32 = self__1.value
    retv161 = t162
    return retv161
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var retv164 int32
    var t165 int32 = self__2.value
    retv164 = t165
    return retv164
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
    var retv171 string
    var t172 string = _goml_runtime_core_int32_to_string(self__6)
    retv171 = t172
    return retv171
}

func println__T_string(value__1 string) struct{} {
    var t174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t174)
    return struct{}{}
}

func combine__A_LeftSource__B_RightSource(left__3 LeftSource, right__4 RightSource) string {
    var retv177 string
    var t178 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(left__3)
    var t179 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t178)
    var t180 string = t179 + ":"
    var t181 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(right__4)
    var t182 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(t181)
    var t183 string = t180 + t182
    retv177 = t183
    return retv177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv185 string
    retv185 = self__38
    return retv185
}

func main() {
    main0()
}
