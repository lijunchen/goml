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
    var x__0 int = 1
    var t111 int = x__0 + 1
    x__0 = t111
    var t112 string = _goml_m_inherent_i_int_i_int_i_to__string(x__0)
    println__T_string(t112)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int_to_string(self__5)
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv120 string
    retv120 = self__38
    return retv120
}

func main() {
    main0()
}
