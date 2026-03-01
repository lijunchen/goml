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
    var retv12 int32
    var t13 int32 = ref_get__Ref_int32(cell__0)
    var t14 int32 = t13 + 1
    ref_set__Ref_int32(cell__0, t14)
    var t15 int32 = ref_get__Ref_int32(cell__0)
    retv12 = t15
    return retv12
}

func flip(flag__1 *ref_bool_x) bool {
    var retv17 bool
    var current__2 bool = ref_get__Ref_bool(flag__1)
    var t18 bool = !current__2
    ref_set__Ref_bool(flag__1, t18)
    var t19 bool = ref_get__Ref_bool(flag__1)
    retv17 = t19
    return retv17
}

func nested_total(cell__3 *ref_ref_int32_x) int32 {
    var retv21 int32
    var inner__4 *ref_int32_x = ref_get__Ref_Ref_int32(cell__3)
    var before__5 int32 = ref_get__Ref_int32(inner__4)
    var t22 int32 = before__5 + 2
    ref_set__Ref_int32(inner__4, t22)
    var t23 int32 = ref_get__Ref_int32(inner__4)
    var t24 int32 = before__5 + t23
    retv21 = t24
    return retv21
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var retv26 int32
    var alias__7 *ref_int32_x = cell__6
    var t27 int32 = ref_get__Ref_int32(alias__7)
    var t28 int32 = t27 + 5
    ref_set__Ref_int32(alias__7, t28)
    var t29 int32 = ref_get__Ref_int32(alias__7)
    retv26 = t29
    return retv26
}

func pair_sum() int32 {
    var retv31 int32
    var first__8 *ref_int32_x = ref__Ref_int32(4)
    var second__9 *ref_int32_x = ref__Ref_int32(6)
    var t32 int32 = ref_get__Ref_int32(first__8)
    var t33 int32 = ref_get__Ref_int32(second__9)
    var t34 int32 = t32 + t33
    ref_set__Ref_int32(first__8, t34)
    var t35 int32 = ref_get__Ref_int32(first__8)
    var t36 int32 = ref_get__Ref_int32(second__9)
    var t37 int32 = t35 + t36
    retv31 = t37
    return retv31
}

func reassign_nested(nested__10 *ref_ref_int32_x) int32 {
    var retv39 int32
    var inner__11 *ref_int32_x = ref_get__Ref_Ref_int32(nested__10)
    var t40 int32 = ref_get__Ref_int32(inner__11)
    var t41 int32 = t40 + 7
    ref_set__Ref_int32(inner__11, t41)
    var t42 int32 = ref_get__Ref_int32(inner__11)
    retv39 = t42
    return retv39
}

func main0() struct{} {
    var counter__12 *ref_int32_x = ref__Ref_int32(39)
    var toggler__13 *ref_bool_x = ref__Ref_bool(false)
    var t44 *ref_int32_x = ref__Ref_int32(3)
    var nested__14 *ref_ref_int32_x = ref__Ref_Ref_int32(t44)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = ref_get__Ref_Ref_int32(nested__14)
    var t45 int32 = ref_get__Ref_int32(inner__18)
    var t46 int32 = t45 + bumped__15
    ref_set__Ref_int32(inner__18, t46)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t47 int32 = ref_get__Ref_int32(counter__12)
    var t48 int32 = bumped__15 + t47
    var t49 string = int32_to_string(t48)
    string_println(t49)
    var t50 int32 = nested_total_val__19 + alias_total__20
    var t51 int32 = t50 + reassigned__22
    var t52 string = int32_to_string(t51)
    string_println(t52)
    var t53 string = int32_to_string(pair_total__21)
    string_println(t53)
    var t54 bool = flipped__16 && flipped_again__17
    var t55 bool = t54 && bool_check__23
    var t56 string = bool_to_string(t55)
    string_println(t56)
    return struct{}{}
}

func main() {
    main0()
}
