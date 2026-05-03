package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var v__0 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32()
    var v__1 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(v__0, 10)
    var v__2 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(v__1, 20)
    var v__3 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(v__2, 30)
    var first__4 int32 = v__3[0]
    var second__5 int32 = v__3[1]
    var third__6 int32 = v__3[2]
    var len__7 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(v__3)
    var t5 string = int32_to_string(first__4)
    println__T_string(t5)
    var t6 string = int32_to_string(second__5)
    println__T_string(t6)
    var t7 string = int32_to_string(third__6)
    println__T_string(t7)
    var t8 string = int32_to_string(len__7)
    println__T_string(t8)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32() []int32 {
    var retv10 []int32
    var t11 []int32 = nil
    retv10 = t11
    return retv10
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(self__66 []int32, elem__67 int32) []int32 {
    var retv13 []int32
    var t14 []int32 = append(self__66, elem__67)
    retv13 = t14
    return retv13
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(self__73 []int32) int32 {
    var retv16 int32
    var t17 int32 = int32(len(self__73))
    retv16 = t17
    return retv16
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
