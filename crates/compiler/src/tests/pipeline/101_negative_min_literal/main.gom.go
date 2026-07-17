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
    var t64 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t64)
    var b__1 int16 = -32768
    var t65 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t65)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__39 int8) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int8_to_string(self__39)
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__40 int16) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int16_to_string(self__40)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv76 string
    retv76 = self__37
    return retv76
}

func main() {
    main0()
}
