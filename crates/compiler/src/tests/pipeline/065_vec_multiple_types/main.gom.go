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

func main0() struct{} {
    var vi__0 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32()
    var vi__1 []int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(vi__0, 42)
    var val_i__2 int32 = vi__1[0]
    var len_i__3 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(vi__1)
    var vs__4 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string()
    var vs__5 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_string(vs__4, "hello")
    var vs__6 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_string(vs__5, "world")
    var val_s__7 string = vs__6[1]
    var len_s__8 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(vs__6)
    var vb__9 []bool = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool()
    var vb__10 []bool = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_bool(vb__9, true)
    var vb__11 []bool = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_bool(vb__10, false)
    var val_b__12 bool = vb__11[0]
    var len_b__13 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_bool(vb__11)
    var t7 string = int32_to_string(val_i__2)
    println__T_string(t7)
    var t8 string = int32_to_string(len_i__3)
    println__T_string(t8)
    println__T_string(val_s__7)
    var t9 string = int32_to_string(len_s__8)
    println__T_string(t9)
    var t10 string = bool_to_string(val_b__12)
    println__T_string(t10)
    var t11 string = int32_to_string(len_b__13)
    println__T_string(t11)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32() []int32 {
    var retv13 []int32
    var t14 []int32 = nil
    retv13 = t14
    return retv13
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_int32(self__66 []int32, elem__67 int32) []int32 {
    var retv16 []int32
    var t17 []int32 = append(self__66, elem__67)
    retv16 = t17
    return retv16
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_int32(self__73 []int32) int32 {
    var retv19 int32
    var t20 int32 = int32(len(self__73))
    retv19 = t20
    return retv19
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string() []string {
    var retv22 []string
    var t23 []string = nil
    retv22 = t23
    return retv22
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_string(self__66 []string, elem__67 string) []string {
    var retv25 []string
    var t26 []string = append(self__66, elem__67)
    retv25 = t26
    return retv25
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv28 int32
    var t29 int32 = int32(len(self__73))
    retv28 = t29
    return retv28
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool() []bool {
    var retv31 []bool
    var t32 []bool = nil
    retv31 = t32
    return retv31
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_bool(self__66 []bool, elem__67 bool) []bool {
    var retv34 []bool
    var t35 []bool = append(self__66, elem__67)
    retv34 = t35
    return retv34
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_bool(self__73 []bool) int32 {
    var retv37 int32
    var t38 int32 = int32(len(self__73))
    retv37 = t38
    return retv37
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
