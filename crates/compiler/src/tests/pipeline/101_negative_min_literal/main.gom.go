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
    var t25 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t25)
    var b__1 int16 = -32768
    var t26 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t26)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t28 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t28)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv31 string
    var t32 string = _goml_runtime_core_int8_to_string(self__11)
    retv31 = t32
    return retv31
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv34 string
    var t35 string = _goml_runtime_core_int16_to_string(self__12)
    retv34 = t35
    return retv34
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv37 string
    retv37 = self__9
    return retv37
}

func main() {
    main0()
}
