package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__0 int = 1
    var a__1 int = a__0 + 2
    var a__2 int = a__1 + 3
    var a__3 int = a__2 + 4
    var t69 string = _goml_m_inherent_i_int_i_int_i_to__string(a__3)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int_to_string(self__5)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv78 string
    retv78 = self__38
    return retv78
}

func main() {
    main0()
}
