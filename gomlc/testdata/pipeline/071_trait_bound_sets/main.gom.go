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
    var retv111 string
    retv111 = "A"
    return retv111
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv115 string
    retv115 = "C"
    return retv115
}

func main0() struct{} {
    var s__5 S = S{}
    var t117 string = pick_a__T_S(s__5)
    println__T_string(t117)
    var t118 string = bar_it__T_S(s__5)
    println__T_string(t118)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv123 string
    var t124 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv123 = t124
    return retv123
}

func bar_it__T_S(x__4 S) string {
    var retv126 string
    var t127 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func main() {
    main0()
}
