package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
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

func sum_to(limit__0 int32) int32 {
    var acc__1 *ref_int32_x
    var i__2 *ref_int32_x
    var mtmp2 struct{}
    var t11 int32
    var t13 int32
    var t14 bool
    var current__3 int32
    var t15 int32
    var t16 int32
    var mtmp0 struct{}
    var t17 int32
    var mtmp1 struct{}
    _ = mtmp2
    _ = mtmp0
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            acc__1 = ref__Ref_int32(0)
            i__2 = ref__Ref_int32(0)
            pc = 2
        case 1:
            t11 = ref_get__Ref_int32(acc__1)
            return t11
        case 2:
            t13 = ref_get__Ref_int32(i__2)
            t14 = t13 < limit__0
            if t14 {
                pc = 3
            } else {
                pc = 4
            }
        case 3:
            current__3 = ref_get__Ref_int32(i__2)
            t15 = ref_get__Ref_int32(acc__1)
            t16 = t15 + current__3
            ref_set__Ref_int32(acc__1, t16)
            t17 = current__3 + 1
            ref_set__Ref_int32(i__2, t17)
            pc = 2
        case 4:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var i__6 *ref_int32_x
    var is_even__7 *ref_bool_x
    var mtmp5 struct{}
    var t19 int32
    var t21 int32
    var t22 bool
    var current__8 int32
    var t23 int32
    var mtmp3 struct{}
    var add_now__9 bool
    var t24 bool
    var mtmp4 struct{}
    var t26 int32
    var t27 int32
    var t28 struct{}
    _ = mtmp5
    _ = mtmp3
    _ = mtmp4
    _ = t28
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            acc__5 = ref__Ref_int32(0)
            i__6 = ref__Ref_int32(0)
            is_even__7 = ref__Ref_bool(true)
            pc = 2
        case 1:
            t19 = ref_get__Ref_int32(acc__5)
            return t19
        case 2:
            t21 = ref_get__Ref_int32(i__6)
            t22 = t21 < limit__4
            if t22 {
                pc = 3
            } else {
                pc = 7
            }
        case 3:
            current__8 = ref_get__Ref_int32(i__6)
            t23 = current__8 + 1
            ref_set__Ref_int32(i__6, t23)
            add_now__9 = ref_get__Ref_bool(is_even__7)
            t24 = !add_now__9
            ref_set__Ref_bool(is_even__7, t24)
            if add_now__9 {
                pc = 5
            } else {
                pc = 6
            }
        case 4:
            pc = 2
        case 5:
            t26 = ref_get__Ref_int32(acc__5)
            t27 = t26 + current__8
            ref_set__Ref_int32(acc__5, t27)
            pc = 4
        case 6:
            pc = 4
        case 7:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var first__10 int32
    var evens__11 int32
    var mtmp6 struct{}
    var mtmp7 struct{}
    var mtmp8 struct{}
    var mtmp9 struct{}
    _ = mtmp6
    _ = mtmp7
    _ = mtmp8
    _ = mtmp9
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            first__10 = sum_to(5)
            evens__11 = sum_even(6)
            print__T_string("sum_to(5)=")
            println__T_int32(first__10)
            print__T_string("sum_even(6)=")
            println__T_int32(evens__11)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func print__T_string(value__0 string) struct{} {
    var t29 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t29 = string_print(value__0)
            return t29
        default:
            panic("invalid pc")
        }
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t30 string
    var t31 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t30 = int32_to_string(value__1)
            t31 = string_println(t30)
            return t31
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
