package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int32 = 1
    var y__1 int8 = 1
    print__T_string("int32: ")
    println__T_int32(x__0)
    print__T_string("int8: ")
    println__T_int8(y__1)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t64)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv73 string
    retv73 = self__34
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__38)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__36 int8) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int8_to_string(self__36)
    retv78 = t79
    return retv78
}

func main() {
    main0()
}
