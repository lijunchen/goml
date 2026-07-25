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
    var retv67 string
    retv67 = "A"
    return retv67
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv71 string
    retv71 = "C"
    return retv71
}

func main0() struct{} {
    var s__5 S = S{}
    var t73 string = pick_a__T_S(s__5)
    println__T_string(t73)
    var t74 string = bar_it__T_S(s__5)
    println__T_string(t74)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv79 string
    var t80 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv79 = t80
    return retv79
}

func bar_it__T_S(x__4 S) string {
    var retv82 string
    var t83 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv85 string
    retv85 = self__38
    return retv85
}

func main() {
    main0()
}
