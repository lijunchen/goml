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
    var retv6 string
    var t7 int32 = self__0.value
    var t8 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t7)
    var t9 string = "S(" + t8
    var t10 string = t9 + ")"
    retv6 = t10
    return retv6
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t12 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t12)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv14 string
    var t15 string = _goml_runtime_core_int32_to_string(self__2)
    retv14 = t15
    return retv14
}

func println__T_string(value__1 string) struct{} {
    var t17 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t17)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv20 string
    retv20 = self__9
    return retv20
}

func main() {
    main0()
}
