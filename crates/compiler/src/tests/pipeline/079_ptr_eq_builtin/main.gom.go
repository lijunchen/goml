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

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ptr_eq__Ref_5int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func main0() struct{} {
    var a__0 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(1)
    var b__1 *ref_int32_x = a__0
    var c__2 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(1)
    var t3 bool = ptr_eq__Ref_5int32(a__0, b__1)
    println__T_bool(t3)
    var t4 bool = ptr_eq__Ref_5int32(a__0, c__2)
    println__T_bool(t4)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(value__93 int32) *ref_int32_x {
    var retv6 *ref_int32_x
    var t7 *ref_int32_x = ref__Ref_5int32(value__93)
    retv6 = t7
    return retv6
}

func println__T_bool(value__1 bool) struct{} {
    var t9 string = bool_to_string(value__1)
    string_println(t9)
    return struct{}{}
}

func main() {
    main0()
}
