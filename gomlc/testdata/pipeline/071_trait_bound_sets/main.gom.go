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
    var t191 string
    var inline213 string = _goml_m_trait__impl_i_A_i_S_i_foo(s__5)
    t191 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline210)
    var t192 string
    var inline208 string = _goml_m_trait__impl_i_C_i_S_i_bar(s__5)
    t192 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
