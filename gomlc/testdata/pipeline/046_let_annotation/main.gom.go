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
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t114)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv123 string
    retv123 = self__38
    return retv123
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int32_to_string(self__43)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv128 string
    var t129 string = _goml_runtime_core_int8_to_string(self__41)
    retv128 = t129
    return retv128
}

func main() {
    main0()
}
