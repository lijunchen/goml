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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var i__0 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    Loop_loop8:
    for {
        var t9 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__0)
        var t10 bool = t9 < 8
        if t10 {
            var t11 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__0)
            var t12 int32 = t11 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__0, t12)
            var t18 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__0)
            var t19 bool = t18 == 3
            if t19 {
                continue
            } else {
                var t16 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__0)
                var t17 bool = t16 == 6
                if t17 {
                    continue
                } else {
                    var t15 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__0)
                    println__T_int32(t15)
                    continue
                }
            }
        } else {
            break Loop_loop8
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(value__93 int32) *ref_int32_x {
    var retv21 *ref_int32_x
    var t22 *ref_int32_x = ref__Ref_5int32(value__93)
    retv21 = t22
    return retv21
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(self__94 *ref_int32_x) int32 {
    var retv24 int32
    var t25 int32 = ref_get__Ref_5int32(self__94)
    retv24 = t25
    return retv24
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t29 string = int32_to_string(value__1)
    string_println(t29)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
