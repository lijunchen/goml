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
    var retv16 int8
    retv16 = 90
    return retv16
}

func b_value() int8 {
    var retv18 int8
    var t19 int8 = -20
    retv18 = t19
    return retv18
}

func c_value() int8 {
    var retv21 int8
    retv21 = 3
    return retv21
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t23 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t24 string = label__0 + t23
    println__T_string(t24)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t26 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t27 string = label__2 + t26
    println__T_string(t27)
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
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int8_to_string(self__11)
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv36 string
    var t37 string = _goml_runtime_core_bool_to_string(self__8)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
