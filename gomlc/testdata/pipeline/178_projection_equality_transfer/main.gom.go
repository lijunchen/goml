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
    var t190 string
    var inline219 string = _goml_runtime_core_int32_to_string(self__0)
    t190 = inline219
    var t191 string = "m" + t190
    return t191
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t194 int32 = self__1.value
    return t194
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t197 int32 = self__2.value
    return t197
}

func main0() struct{} {
    var t199 LeftSource = LeftSource{
        value: 3,
    }
    var t200 RightSource = RightSource{
        value: 4,
    }
    var t201 string
    var inline224 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t199)
    var inline225 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline224)
    var inline226 string = inline225 + ":"
    var inline227 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t200)
    var inline228 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline227)
    var inline229 string = inline226 + inline228
    t201 = inline229
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
