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

type S struct {
    value int32
}

func main0() struct{} {
    var t195 string
    var inline210 int32 = 7
    var inline211 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline210)
    var inline212 string = "S(" + inline211
    var inline213 string = inline212 + ")"
    t195 = inline213
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t198 string = _goml_runtime_core_int32_to_string(self__33)
    return t198
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
