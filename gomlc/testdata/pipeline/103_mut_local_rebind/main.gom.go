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
    var t158 int = x__0 + 1
    x__0 = t158
    var t159 string = _goml_m_inherent_i_int_i_int_i_to__string(x__0)
    println__T_string(t159)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t161 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t165 string = _goml_runtime_core_int_to_string(self__5)
    return t165
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
