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
    var retv155 string
    retv155 = "A"
    return retv155
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv159 string
    retv159 = "C"
    return retv159
}

func main0() struct{} {
    var s__5 S = S{}
    var t161 string = pick_a__T_S(s__5)
    println__T_string(t161)
    var t162 string = bar_it__T_S(s__5)
    println__T_string(t162)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t164)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv167 string
    var t168 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv167 = t168
    return retv167
}

func bar_it__T_S(x__4 S) string {
    var retv170 string
    var t171 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv173 string
    retv173 = self__38
    return retv173
}

func main() {
    main0()
}
