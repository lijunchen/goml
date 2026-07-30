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

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var retv110 string
    var t111 int32 = self__0.value
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    var t113 string = "S(" + t112
    var t114 string = t113 + ")"
    retv110 = t114
    return retv110
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t116 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t116)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__6)
    retv118 = t119
    return retv118
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func main() {
    main0()
}
