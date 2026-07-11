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
    var retv10 string
    retv10 = "A"
    return retv10
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv14 string
    retv14 = "C"
    return retv14
}

func main0() struct{} {
    var s__5 S = S{}
    var t16 string = pick_a__T_S(s__5)
    println__T_string(t16)
    var t17 string = bar_it__T_S(s__5)
    println__T_string(t17)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t19 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t19)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv22 string
    var t23 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv22 = t23
    return retv22
}

func bar_it__T_S(x__4 S) string {
    var retv25 string
    var t26 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv25 = t26
    return retv25
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv28 string
    retv28 = self__9
    return retv28
}

func main() {
    main0()
}
