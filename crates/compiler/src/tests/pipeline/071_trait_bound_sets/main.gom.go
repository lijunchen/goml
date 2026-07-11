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
    var retv25 string
    retv25 = "A"
    return retv25
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv29 string
    retv29 = "C"
    return retv29
}

func main0() struct{} {
    var s__5 S = S{}
    var t31 string = pick_a__T_S(s__5)
    println__T_string(t31)
    var t32 string = bar_it__T_S(s__5)
    println__T_string(t32)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv37 string
    var t38 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv37 = t38
    return retv37
}

func bar_it__T_S(x__4 S) string {
    var retv40 string
    var t41 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv43 string
    retv43 = self__9
    return retv43
}

func main() {
    main0()
}
