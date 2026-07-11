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
    var retv9 string
    var t10 int32 = self__0.value
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t10)
    var t12 string = "S(" + t11
    var t13 string = t12 + ")"
    retv9 = t13
    return retv9
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t15 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t15)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv17 string
    var t18 string = _goml_runtime_core_int32_to_string(self__2)
    retv17 = t18
    return retv17
}

func println__T_string(value__1 string) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv23 string
    retv23 = self__9
    return retv23
}

func main() {
    main0()
}
