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
    var t175 string
    var inline204 string = _goml_runtime_core_int32_to_string(self__0)
    t175 = inline204
    var t176 string = "m" + t175
    return t176
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t179 int32 = self__1.value
    return t179
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t182 int32 = self__2.value
    return t182
}

func main0() struct{} {
    var t184 LeftSource = LeftSource{
        value: 3,
    }
    var t185 RightSource = RightSource{
        value: 4,
    }
    var t186 string
    var inline209 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t184)
    var inline210 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline209)
    var inline211 string = inline210 + ":"
    var inline212 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t185)
    var inline213 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline212)
    var inline214 string = inline211 + inline213
    t186 = inline214
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
