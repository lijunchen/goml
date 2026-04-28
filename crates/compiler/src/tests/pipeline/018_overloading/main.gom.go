package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func _goml_trait_x5f_impl_x23_Arith_x23_int32_x23_add(self__0 int32, other__1 int32) int32 {
    var retv5 int32
    var t6 int32 = self__0 + other__1
    retv5 = t6
    return retv5
}

func _goml_trait_x5f_impl_x23_Arith_x23_int32_x23_less(self__2 int32, other__3 int32) bool {
    var retv8 bool
    var t9 bool = self__2 < other__3
    retv8 = t9
    return retv8
}

func _goml_trait_x5f_impl_x23_Output_x23_int32_x23_output(self__4 int32) struct{} {
    var t11 string = int32_to_string(self__4)
    string_println(t11)
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Output_x23_bool_x23_output(self__5 bool) struct{} {
    var t14 string = bool_to_string(self__5)
    string_println(t14)
    return struct{}{}
}

func main0() struct{} {
    var a__7 int32 = id__T_int32(1)
    var b__8 int32 = id__T_int32(2)
    var c__9 int32 = _goml_trait_x5f_impl_x23_Arith_x23_int32_x23_add(a__7, b__8)
    _goml_trait_x5f_impl_x23_Output_x23_int32_x23_output(c__9)
    var a__10 int32 = id__T_int32(3)
    var b__11 int32 = id__T_int32(4)
    var c__12 bool = _goml_trait_x5f_impl_x23_Arith_x23_int32_x23_less(a__10, b__11)
    _goml_trait_x5f_impl_x23_Output_x23_bool_x23_output(c__12)
    id__T_string("abc")
    id__T_bool(true)
    return struct{}{}
}

func id__T_int32(x__6 int32) int32 {
    var retv18 int32
    retv18 = x__6
    return retv18
}

func id__T_string(x__6 string) string {
    var retv20 string
    retv20 = x__6
    return retv20
}

func id__T_bool(x__6 bool) bool {
    var retv22 bool
    retv22 = x__6
    return retv22
}

func main() {
    main0()
}
