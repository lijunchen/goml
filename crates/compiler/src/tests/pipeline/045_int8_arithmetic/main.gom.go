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
    var retv76 int8
    retv76 = 90
    return retv76
}

func b_value() int8 {
    var retv78 int8
    retv78 = -20
    return retv78
}

func c_value() int8 {
    var retv80 int8
    retv80 = 3
    return retv80
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t83 string = label__0 + t82
    println__T_string(t83)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t86 string = label__2 + t85
    println__T_string(t86)
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
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int8_to_string(self__41)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv95 string
    var t96 string = _goml_runtime_core_bool_to_string(self__37)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
