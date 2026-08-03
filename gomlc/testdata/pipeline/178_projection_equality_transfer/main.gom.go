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
    var t139 string
    var inline168 string = _goml_runtime_core_int32_to_string(self__0)
    t139 = inline168
    var t140 string = "m" + t139
    return t140
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t143 int32 = self__1.value
    return t143
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t146 int32 = self__2.value
    return t146
}

func main0() struct{} {
    var t148 LeftSource = LeftSource{
        value: 3,
    }
    var t149 RightSource = RightSource{
        value: 4,
    }
    var t150 string
    var inline173 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t148)
    var inline174 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline173)
    var inline175 string = inline174 + ":"
    var inline176 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t149)
    var inline177 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline176)
    var inline178 string = inline175 + inline177
    t150 = inline178
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
