package main

import (
    _goml_fmt "fmt"
    "path"
    "strings"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var parts__0 []string = strings.Fields("alpha beta gamma")
    var t2 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(parts__0)
    var t3 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_string(parts__0, 0, t2)
    var t4 string = path.Join(t3...)
    println__T_string(t4)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__74 []string) int32 {
    var retv8 int32
    var t9 int32 = int32(len(self__74))
    retv8 = t9
    return retv8
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_string(self__75 []string, start__76 int32, end__77 int32) []string {
    var retv11 []string
    var t12 []string = self__75[start__76:end__77]
    retv11 = t12
    return retv11
}

func main() {
    main0()
}
