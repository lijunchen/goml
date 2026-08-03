package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_foo(self__0 S) string {
    return "A"
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    return "C"
}

func main0() struct{} {
    var s__5 S = S{}
    var t145 string
    var inline167 string = _goml_m_trait__impl_i_A_i_S_i_foo(s__5)
    t145 = inline167
    var inline164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline164)
    var t146 string
    var inline162 string = _goml_m_trait__impl_i_C_i_S_i_bar(s__5)
    t146 = inline162
    var inline159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline159)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
