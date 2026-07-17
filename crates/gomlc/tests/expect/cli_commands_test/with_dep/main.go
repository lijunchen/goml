package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_Lib_p_msg() string {
    var retv62 string
    retv62 = "hi"
    return retv62
}

func main0() struct{} {
    var t64 string = _goml_m_Lib_p_msg()
    println__T_string(t64)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv70 string
    retv70 = self__37
    return retv70
}

func main() {
    main0()
}
