package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_Lib_p_msg() string {
    var retv8 string
    retv8 = "hi"
    return retv8
}

func main0() struct{} {
    var t10 string = _goml_m_Lib_p_msg()
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t13 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t13)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv16 string
    retv16 = self__9
    return retv16
}

func main() {
    main0()
}
