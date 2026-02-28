package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_ref_int32_x struct {
    value *ref_int32_x
}

func ref__Ref_Ref_int32(value *ref_int32_x) *ref_ref_int32_x {
    return &ref_ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_Ref_int32(reference *ref_ref_int32_x) *ref_int32_x {
    return reference.value
}

func bump(cell__0 *ref_int32_x) int32 {
    var t11 int32 = ref_get__Ref_int32(cell__0)
    var t12 int32 = t11 + 1
    ref_set__Ref_int32(cell__0, t12)
    var t13 int32 = ref_get__Ref_int32(cell__0)
    return t13
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool = ref_get__Ref_bool(flag__1)
    var t14 bool = !current__2
    ref_set__Ref_bool(flag__1, t14)
    var t15 bool = ref_get__Ref_bool(flag__1)
    return t15
}

func nested_total(cell__3 *ref_ref_int32_x) int32 {
    var inner__4 *ref_int32_x = ref_get__Ref_Ref_int32(cell__3)
    var before__5 int32 = ref_get__Ref_int32(inner__4)
    var t16 int32 = before__5 + 2
    ref_set__Ref_int32(inner__4, t16)
    var t17 int32 = ref_get__Ref_int32(inner__4)
    var t18 int32 = before__5 + t17
    return t18
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var alias__7 *ref_int32_x = cell__6
    var t19 int32 = ref_get__Ref_int32(alias__7)
    var t20 int32 = t19 + 5
    ref_set__Ref_int32(alias__7, t20)
    var t21 int32 = ref_get__Ref_int32(alias__7)
    return t21
}

func pair_sum() int32 {
    var first__8 *ref_int32_x = ref__Ref_int32(4)
    var second__9 *ref_int32_x = ref__Ref_int32(6)
    var t22 int32 = ref_get__Ref_int32(first__8)
    var t23 int32 = ref_get__Ref_int32(second__9)
    var t24 int32 = t22 + t23
    ref_set__Ref_int32(first__8, t24)
    var t25 int32 = ref_get__Ref_int32(first__8)
    var t26 int32 = ref_get__Ref_int32(second__9)
    var t27 int32 = t25 + t26
    return t27
}

func reassign_nested(nested__10 *ref_ref_int32_x) int32 {
    var inner__11 *ref_int32_x = ref_get__Ref_Ref_int32(nested__10)
    var t28 int32 = ref_get__Ref_int32(inner__11)
    var t29 int32 = t28 + 7
    ref_set__Ref_int32(inner__11, t29)
    var t30 int32 = ref_get__Ref_int32(inner__11)
    return t30
}

func main0() struct{} {
    var counter__12 *ref_int32_x = ref__Ref_int32(39)
    var toggler__13 *ref_bool_x = ref__Ref_bool(false)
    var t31 *ref_int32_x = ref__Ref_int32(3)
    var nested__14 *ref_ref_int32_x = ref__Ref_Ref_int32(t31)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = ref_get__Ref_Ref_int32(nested__14)
    var t32 int32 = ref_get__Ref_int32(inner__18)
    var t33 int32 = t32 + bumped__15
    ref_set__Ref_int32(inner__18, t33)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t34 int32 = ref_get__Ref_int32(counter__12)
    var t35 int32 = bumped__15 + t34
    var t36 string = int32_to_string(t35)
    string_println(t36)
    var t37 int32 = nested_total_val__19 + alias_total__20
    var t38 int32 = t37 + reassigned__22
    var t39 string = int32_to_string(t38)
    string_println(t39)
    var t40 string = int32_to_string(pair_total__21)
    string_println(t40)
    var t41 bool = flipped__16 && flipped_again__17
    var t42 bool = t41 && bool_check__23
    var t43 string = bool_to_string(t42)
    string_println(t43)
    return struct{}{}
}

func main() {
    main0()
}
