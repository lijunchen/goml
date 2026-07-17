package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func a_value() int8 {
    var retv73 int8
    retv73 = 90
    return retv73
}

func b_value() int8 {
    var retv75 int8
    var t76 int8 = -20
    retv75 = t76
    return retv75
}

func c_value() int8 {
    var retv78 int8
    retv78 = 3
    return retv78
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t81 string = label__0 + t80
    println__T_string(t81)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t84 string = label__2 + t83
    println__T_string(t84)
    return struct{}{}
}

func main0() struct{} {
    var a__4 int8 = a_value()
    var b__5 int8 = b_value()
    var c__6 int8 = c_value()
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    show_int8("a=", a__4)
    show_int8("b=", b__5)
    show_int8("c=", c__6)
    show_int8("sum=", sum__7)
    show_int8("diff=", diff__8)
    show_int8("prod=", prod__9)
    show_int8("quot=", quot__10)
    show_int8("neg=", neg__11)
    show_bool("b<a=", less__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__39 int8) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int8_to_string(self__39)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv93 string
    var t94 string = _goml_runtime_core_bool_to_string(self__36)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv96 string
    retv96 = self__37
    return retv96
}

func main() {
    main0()
}
