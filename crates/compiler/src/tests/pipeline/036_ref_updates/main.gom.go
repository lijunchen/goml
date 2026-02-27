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
    var ret64 int32
    var t38 int32 = ref_get__Ref_int32(cell__0)
    var t37 int32 = t38 + 1
    ref_set__Ref_int32(cell__0, t37)
    ret64 = ref_get__Ref_int32(cell__0)
    return ret64
}

func flip(flag__1 *ref_bool_x) bool {
    var ret65 bool
    var current__2 bool = ref_get__Ref_bool(flag__1)
    var t39 bool = !current__2
    ref_set__Ref_bool(flag__1, t39)
    ret65 = ref_get__Ref_bool(flag__1)
    return ret65
}

func nested_total(cell__3 *ref_ref_int32_x) int32 {
    var ret66 int32
    var inner__4 *ref_int32_x = ref_get__Ref_Ref_int32(cell__3)
    var before__5 int32 = ref_get__Ref_int32(inner__4)
    var t40 int32 = before__5 + 2
    ref_set__Ref_int32(inner__4, t40)
    var t41 int32 = ref_get__Ref_int32(inner__4)
    ret66 = before__5 + t41
    return ret66
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var ret67 int32
    var alias__7 *ref_int32_x = cell__6
    var t43 int32 = ref_get__Ref_int32(alias__7)
    var t42 int32 = t43 + 5
    ref_set__Ref_int32(alias__7, t42)
    ret67 = ref_get__Ref_int32(alias__7)
    return ret67
}

func pair_sum() int32 {
    var ret68 int32
    var first__8 *ref_int32_x = ref__Ref_int32(4)
    var second__9 *ref_int32_x = ref__Ref_int32(6)
    var t45 int32 = ref_get__Ref_int32(first__8)
    var t46 int32 = ref_get__Ref_int32(second__9)
    var t44 int32 = t45 + t46
    ref_set__Ref_int32(first__8, t44)
    var t47 int32 = ref_get__Ref_int32(first__8)
    var t48 int32 = ref_get__Ref_int32(second__9)
    ret68 = t47 + t48
    return ret68
}

func reassign_nested(nested__10 *ref_ref_int32_x) int32 {
    var ret69 int32
    var inner__11 *ref_int32_x = ref_get__Ref_Ref_int32(nested__10)
    var t50 int32 = ref_get__Ref_int32(inner__11)
    var t49 int32 = t50 + 7
    ref_set__Ref_int32(inner__11, t49)
    ret69 = ref_get__Ref_int32(inner__11)
    return ret69
}

func main0() struct{} {
    var ret70 struct{}
    var counter__12 *ref_int32_x = ref__Ref_int32(39)
    var toggler__13 *ref_bool_x = ref__Ref_bool(false)
    var t51 *ref_int32_x = ref__Ref_int32(3)
    var nested__14 *ref_ref_int32_x = ref__Ref_Ref_int32(t51)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = ref_get__Ref_Ref_int32(nested__14)
    var t53 int32 = ref_get__Ref_int32(inner__18)
    var t52 int32 = t53 + bumped__15
    ref_set__Ref_int32(inner__18, t52)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32 = alias_bump(counter__12)
    var pair_total__21 int32 = pair_sum()
    var reassigned__22 int32 = reassign_nested(nested__14)
    var bool_check__23 bool = !false
    var t56 int32 = ref_get__Ref_int32(counter__12)
    var t55 int32 = bumped__15 + t56
    var t54 string = int32_to_string(t55)
    string_println(t54)
    var t59 int32 = nested_total_val__19 + alias_total__20
    var t58 int32 = t59 + reassigned__22
    var t57 string = int32_to_string(t58)
    string_println(t57)
    var t60 string = int32_to_string(pair_total__21)
    string_println(t60)
    var t63 bool = flipped__16 && flipped_again__17
    var t62 bool = t63 && bool_check__23
    var t61 string = bool_to_string(t62)
    string_println(t61)
    ret70 = struct{}{}
    return ret70
}

func main() {
    main0()
}
