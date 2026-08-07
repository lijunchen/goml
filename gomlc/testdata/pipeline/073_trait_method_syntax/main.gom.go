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
    var t180 string
    var inline195 int32 = 7
    var inline196 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline195)
    var inline197 string = "S(" + inline196
    var inline198 string = inline197 + ")"
    t180 = inline198
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline192)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t183 string = _goml_runtime_core_int32_to_string(self__35)
    return t183
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
