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
    var t67 int = x__0 + 1
    x__0 = t67
    var t68 string = _goml_m_inherent_i_int_i_int_i_to__string(x__0)
    println__T_string(t68)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int_to_string(self__5)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv76 string
    retv76 = self__38
    return retv76
}

func main() {
    main0()
}
