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
    var v__4 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(v__3, 40)
    var s__5 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_int32(v__4, 1, 4)
    var t8 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(s__5)
    println__T_int32(t8)
    var t9 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(s__5, 0)
    println__T_int32(t9)
    var t10 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(s__5, 1)
    println__T_int32(t10)
    var t11 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(s__5, 2)
    println__T_int32(t11)
    var t__6 []int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_sub_x5f__x5f_T_x5f_int32(s__5, 1, 3)
    var t12 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(t__6)
    println__T_int32(t12)
    var t13 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(t__6, 0)
    println__T_int32(t13)
    var t14 int32 = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(t__6, 1)
    println__T_int32(t14)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32() []int32 {
    var retv16 []int32
    var t17 []int32 = nil
    retv16 = t17
    return retv16
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(self__66 []int32, elem__67 int32) []int32 {
    var retv19 []int32
    var t20 []int32 = append(self__66, elem__67)
    retv19 = t20
    return retv19
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_int32(self__74 []int32, start__75 int32, end__76 int32) []int32 {
    var retv22 []int32
    var t23 []int32 = self__74[start__75:end__76]
    retv22 = t23
    return retv22
}

func println__T_int32(value__1 int32) struct{} {
    var t25 string = int32_to_string(value__1)
    string_println(t25)
    return struct{}{}
}

func _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(self__79 []int32) int32 {
    var retv28 int32
    var t29 int32 = int32(len(self__79))
    retv28 = t29
    return retv28
}

func _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(self__77 []int32, index__78 int32) int32 {
    var retv31 int32
    var t32 int32 = self__77[index__78]
    retv31 = t32
    return retv31
}

func _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_sub_x5f__x5f_T_x5f_int32(self__80 []int32, start__81 int32, end__82 int32) []int32 {
    var retv34 []int32
    var t35 []int32 = self__80[start__81:end__82]
    retv34 = t35
    return retv34
}

func main() {
    main0()
}
