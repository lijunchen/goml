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
    var t196 string
    var inline218 string = _goml_m_trait__impl_i_A_i_S_i_foo(s__5)
    t196 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline215)
    var t197 string
    var inline213 string = _goml_m_trait__impl_i_C_i_S_i_bar(s__5)
    t197 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
