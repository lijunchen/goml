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
    var t180 string
    var inline209 string = _goml_runtime_core_int32_to_string(self__0)
    t180 = inline209
    var t181 string = "m" + t180
    return t181
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t184 int32 = self__1.value
    return t184
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t187 int32 = self__2.value
    return t187
}

func main0() struct{} {
    var t189 LeftSource = LeftSource{
        value: 3,
    }
    var t190 RightSource = RightSource{
        value: 4,
    }
    var t191 string
    var inline214 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t189)
    var inline215 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline214)
    var inline216 string = inline215 + ":"
    var inline217 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t190)
    var inline218 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline217)
    var inline219 string = inline216 + inline218
    t191 = inline219
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
