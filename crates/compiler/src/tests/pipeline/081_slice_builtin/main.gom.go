package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var v__0 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
    var v__1 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__0, 10)
    var v__2 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__1, 20)
    var v__3 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__2, 30)
    var v__4 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__3, 40)
    var s__5 []int32 = v__4[1:4]
    var t8 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(s__5)
    println__T_int32(t8)
    var t9 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 0)
    println__T_int32(t9)
    var t10 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 1)
    println__T_int32(t10)
    var t11 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 2)
    println__T_int32(t11)
    var t__6 []int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__sub__T_int32(s__5, 1, 3)
    var t12 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(t__6)
    println__T_int32(t12)
    var t13 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(t__6, 0)
    println__T_int32(t13)
    var t14 int32 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(t__6, 1)
    println__T_int32(t14)
    return struct{}{}
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var retv16 []int32
    var t17 []int32 = nil
    retv16 = t17
    return retv16
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__67 []int32, elem__68 int32) []int32 {
    var retv19 []int32
    var t20 []int32 = append(self__67, elem__68)
    retv19 = t20
    return retv19
}

func println__T_int32(value__1 int32) struct{} {
    var t22 string = int32_to_string(value__1)
    string_println(t22)
    return struct{}{}
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(self__77 []int32) int32 {
    var retv25 int32
    var t26 int32 = int32(len(self__77))
    retv25 = t26
    return retv25
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(self__75 []int32, index__76 int32) int32 {
    var retv28 int32
    var t29 int32 = self__75[index__76]
    retv28 = t29
    return retv28
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__sub__T_int32(self__78 []int32, start__79 int32, end__80 int32) []int32 {
    var retv31 []int32
    var t32 []int32 = self__78[start__79:end__80]
    retv31 = t32
    return retv31
}

func main() {
    main0()
}
