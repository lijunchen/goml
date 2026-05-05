package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

func sum_to(limit__0 int32) int32 {
    var retv11 int32
    var acc__1 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var i__2 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    Loop_loop14:
    for {
        var t15 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__2)
        var t16 bool = t15 < limit__0
        if t16 {
            var current__3 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__2)
            var t17 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__1)
            var t18 int32 = t17 + current__3
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(acc__1, t18)
            var t19 int32 = current__3 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__2, t19)
            continue
        } else {
            break Loop_loop14
        }
    }
    var t13 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__1)
    retv11 = t13
    return retv11
}

func sum_even(limit__4 int32) int32 {
    var retv21 int32
    var acc__5 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var i__6 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var is_even__7 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(true)
    Loop_loop24:
    for {
        var t25 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__6)
        var t26 bool = t25 < limit__4
        if t26 {
            var current__8 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__6)
            var t27 int32 = current__8 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__6, t27)
            var add_now__9 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(is_even__7)
            var t28 bool = !add_now__9
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(is_even__7, t28)
            if add_now__9 {
                var t30 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__5)
                var t31 int32 = t30 + current__8
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(acc__5, t31)
            } else {}
            continue
        } else {
            break Loop_loop24
        }
    }
    var t23 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__5)
    retv21 = t23
    return retv21
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    print__T_string("sum_to(5)=")
    println__T_int32(first__10)
    print__T_string("sum_even(6)=")
    println__T_int32(evens__11)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(value__93 int32) *ref_int32_x {
    var retv35 *ref_int32_x
    var t36 *ref_int32_x = ref__Ref_5int32(value__93)
    retv35 = t36
    return retv35
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(self__94 *ref_int32_x) int32 {
    var retv38 int32
    var t39 int32 = ref_get__Ref_5int32(self__94)
    retv38 = t39
    return retv38
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(value__93 bool) *ref_bool_x {
    var retv43 *ref_bool_x
    var t44 *ref_bool_x = ref__Ref_4bool(value__93)
    retv43 = t44
    return retv43
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(self__94 *ref_bool_x) bool {
    var retv46 bool
    var t47 bool = ref_get__Ref_4bool(self__94)
    retv46 = t47
    return retv46
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(self__95 *ref_bool_x, value__96 bool) struct{} {
    ref_set__Ref_4bool(self__95, value__96)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t53 string = int32_to_string(value__1)
    string_println(t53)
    return struct{}{}
}

func main() {
    main0()
}
