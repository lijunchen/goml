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
    var retv7 string
    retv7 = "A"
    return retv7
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv11 string
    retv11 = "C"
    return retv11
}

func main0() struct{} {
    var s__5 S = S{}
    var t13 string = pick_a__T_S(s__5)
    println__T_string(t13)
    var t14 string = bar_it__T_S(s__5)
    println__T_string(t14)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t16 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t16)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv19 string
    var t20 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv19 = t20
    return retv19
}

func bar_it__T_S(x__4 S) string {
    var retv22 string
    var t23 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv22 = t23
    return retv22
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
