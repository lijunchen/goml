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
    var t185 string
    var inline200 int32 = 7
    var inline201 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline200)
    var inline202 string = "S(" + inline201
    var inline203 string = inline202 + ")"
    t185 = inline203
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__35)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
