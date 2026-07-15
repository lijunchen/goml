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
    var t28 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t28)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv37 string
    retv37 = self__9
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__13)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int8_to_string(self__11)
    retv42 = t43
    return retv42
}

func main() {
    main0()
}
