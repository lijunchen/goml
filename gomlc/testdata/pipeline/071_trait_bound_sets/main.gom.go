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
    var t164 string = pick_a__T_S(s__5)
    println__T_string(t164)
    var t165 string = bar_it__T_S(s__5)
    println__T_string(t165)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var t171 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    return t171
}

func bar_it__T_S(x__4 S) string {
    var t174 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
