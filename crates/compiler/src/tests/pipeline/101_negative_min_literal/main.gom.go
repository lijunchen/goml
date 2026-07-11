package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__0 int8 = -128
    var t7 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t7)
    var b__1 int16 = -32768
    var t8 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t8)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t10 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t10)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv13 string
    var t14 string = _goml_runtime_core_int8_to_string(self__11)
    retv13 = t14
    return retv13
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv16 string
    var t17 string = _goml_runtime_core_int16_to_string(self__12)
    retv16 = t17
    return retv16
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv19 string
    retv19 = self__9
    return retv19
}

func main() {
    main0()
}
