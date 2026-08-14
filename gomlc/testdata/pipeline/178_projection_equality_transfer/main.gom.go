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
    var t185 string
    var inline214 string = _goml_runtime_core_int32_to_string(self__0)
    t185 = inline214
    var t186 string = "m" + t185
    return t186
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t189 int32 = self__1.value
    return t189
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t192 int32 = self__2.value
    return t192
}

func main0() struct{} {
    var t194 LeftSource = LeftSource{
        value: 3,
    }
    var t195 RightSource = RightSource{
        value: 4,
    }
    var t196 string
    var inline219 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t194)
    var inline220 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline219)
    var inline221 string = inline220 + ":"
    var inline222 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t195)
    var inline223 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline222)
    var inline224 string = inline221 + inline223
    t196 = inline224
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
