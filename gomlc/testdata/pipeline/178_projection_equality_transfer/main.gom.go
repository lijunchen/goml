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
    var t158 string
    var inline187 string = _goml_runtime_core_int32_to_string(self__0)
    t158 = inline187
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
    var t169 string
    var inline192 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t167)
    var inline193 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline192)
    var inline194 string = inline193 + ":"
    var inline195 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t168)
    var inline196 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline195)
    var inline197 string = inline194 + inline196
    t169 = inline197
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
