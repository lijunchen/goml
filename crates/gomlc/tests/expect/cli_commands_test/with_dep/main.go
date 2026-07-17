package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_Lib_p_msg() string {
    var retv59 string
    retv59 = "hi"
    return retv59
}

func main0() struct{} {
    var t61 string = _goml_m_Lib_p_msg()
    println__T_string(t61)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv67 string
    retv67 = self__34
    return retv67
}

func main() {
    main0()
}
