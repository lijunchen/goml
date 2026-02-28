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
    var t11 int32
    var t12 int32
    var mtmp0 struct{}
    var t13 int32
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = ref_get__Ref_int32(cell__0)
            t12 = t11 + 1
            ref_set__Ref_int32(cell__0, t12)
            t13 = ref_get__Ref_int32(cell__0)
            return t13
        default:
            panic("invalid pc")
        }
    }
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var t14 bool
    var mtmp1 struct{}
    var t15 bool
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            current__2 = ref_get__Ref_bool(flag__1)
            t14 = !current__2
            ref_set__Ref_bool(flag__1, t14)
            t15 = ref_get__Ref_bool(flag__1)
            return t15
        default:
            panic("invalid pc")
        }
    }
}

func nested_total(cell__3 *ref_ref_int32_x) int32 {
    var inner__4 *ref_int32_x
    var before__5 int32
    var t16 int32
    var mtmp2 struct{}
    var t17 int32
    var t18 int32
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            inner__4 = ref_get__Ref_Ref_int32(cell__3)
            before__5 = ref_get__Ref_int32(inner__4)
            t16 = before__5 + 2
            ref_set__Ref_int32(inner__4, t16)
            t17 = ref_get__Ref_int32(inner__4)
            t18 = before__5 + t17
            return t18
        default:
            panic("invalid pc")
        }
    }
}

func alias_bump(cell__6 *ref_int32_x) int32 {
    var alias__7 *ref_int32_x
    var t19 int32
    var t20 int32
    var mtmp3 struct{}
    var t21 int32
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            alias__7 = cell__6
            t19 = ref_get__Ref_int32(alias__7)
            t20 = t19 + 5
            ref_set__Ref_int32(alias__7, t20)
            t21 = ref_get__Ref_int32(alias__7)
            return t21
        default:
            panic("invalid pc")
        }
    }
}

func pair_sum() int32 {
    var first__8 *ref_int32_x
    var second__9 *ref_int32_x
    var t22 int32
    var t23 int32
    var t24 int32
    var mtmp4 struct{}
    var t25 int32
    var t26 int32
    var t27 int32
    _ = mtmp4
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            first__8 = ref__Ref_int32(4)
            second__9 = ref__Ref_int32(6)
            t22 = ref_get__Ref_int32(first__8)
            t23 = ref_get__Ref_int32(second__9)
            t24 = t22 + t23
            ref_set__Ref_int32(first__8, t24)
            t25 = ref_get__Ref_int32(first__8)
            t26 = ref_get__Ref_int32(second__9)
            t27 = t25 + t26
            return t27
        default:
            panic("invalid pc")
        }
    }
}

func reassign_nested(nested__10 *ref_ref_int32_x) int32 {
    var inner__11 *ref_int32_x
    var t28 int32
    var t29 int32
    var mtmp5 struct{}
    var t30 int32
    _ = mtmp5
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            inner__11 = ref_get__Ref_Ref_int32(nested__10)
            t28 = ref_get__Ref_int32(inner__11)
            t29 = t28 + 7
            ref_set__Ref_int32(inner__11, t29)
            t30 = ref_get__Ref_int32(inner__11)
            return t30
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var counter__12 *ref_int32_x
    var toggler__13 *ref_bool_x
    var t31 *ref_int32_x
    var nested__14 *ref_ref_int32_x
    var bumped__15 int32
    var flipped__16 bool
    var flipped_again__17 bool
    var inner__18 *ref_int32_x
    var t32 int32
    var t33 int32
    var mtmp6 struct{}
    var nested_total_val__19 int32
    var alias_total__20 int32
    var pair_total__21 int32
    var reassigned__22 int32
    var bool_check__23 bool
    var t34 int32
    var t35 int32
    var t36 string
    var mtmp7 struct{}
    var t37 int32
    var t38 int32
    var t39 string
    var mtmp8 struct{}
    var t40 string
    var mtmp9 struct{}
    var t41 bool
    var t42 bool
    var t43 string
    var mtmp10 struct{}
    _ = mtmp6
    _ = mtmp7
    _ = mtmp8
    _ = mtmp9
    _ = mtmp10
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            counter__12 = ref__Ref_int32(39)
            toggler__13 = ref__Ref_bool(false)
            t31 = ref__Ref_int32(3)
            nested__14 = ref__Ref_Ref_int32(t31)
            bumped__15 = bump(counter__12)
            flipped__16 = flip(toggler__13)
            flipped_again__17 = flip(toggler__13)
            inner__18 = ref_get__Ref_Ref_int32(nested__14)
            t32 = ref_get__Ref_int32(inner__18)
            t33 = t32 + bumped__15
            ref_set__Ref_int32(inner__18, t33)
            nested_total_val__19 = nested_total(nested__14)
            alias_total__20 = alias_bump(counter__12)
            pair_total__21 = pair_sum()
            reassigned__22 = reassign_nested(nested__14)
            bool_check__23 = !false
            t34 = ref_get__Ref_int32(counter__12)
            t35 = bumped__15 + t34
            t36 = int32_to_string(t35)
            string_println(t36)
            t37 = nested_total_val__19 + alias_total__20
            t38 = t37 + reassigned__22
            t39 = int32_to_string(t38)
            string_println(t39)
            t40 = int32_to_string(pair_total__21)
            string_println(t40)
            t41 = flipped__16 && flipped_again__17
            t42 = t41 && bool_check__23
            t43 = bool_to_string(t42)
            string_println(t43)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
