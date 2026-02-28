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
    var acc__1 *ref_int32_x = ref__Ref_int32(0)
    var i__2 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t13 int32 = ref_get__Ref_int32(i__2)
        var t14 bool = t13 < limit__0
        if !t14 {
            break
        }
        var current__3 int32 = ref_get__Ref_int32(i__2)
        var t15 int32 = ref_get__Ref_int32(acc__1)
        var t16 int32 = t15 + current__3
        ref_set__Ref_int32(acc__1, t16)
        var t17 int32 = current__3 + 1
        ref_set__Ref_int32(i__2, t17)
        continue
    }
    var t11 int32 = ref_get__Ref_int32(acc__1)
    return t11
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x = ref__Ref_int32(0)
    var i__6 *ref_int32_x = ref__Ref_int32(0)
    var is_even__7 *ref_bool_x = ref__Ref_bool(true)
    for {
        var t21 int32 = ref_get__Ref_int32(i__6)
        var t22 bool = t21 < limit__4
        if !t22 {
            break
        }
        var current__8 int32 = ref_get__Ref_int32(i__6)
        var t23 int32 = current__8 + 1
        ref_set__Ref_int32(i__6, t23)
        var add_now__9 bool = ref_get__Ref_bool(is_even__7)
        var t24 bool = !add_now__9
        ref_set__Ref_bool(is_even__7, t24)
        if add_now__9 {
            var t26 int32 = ref_get__Ref_int32(acc__5)
            var t27 int32 = t26 + current__8
            ref_set__Ref_int32(acc__5, t27)
        } else {}
        continue
    }
    var t19 int32 = ref_get__Ref_int32(acc__5)
    return t19
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

func print__T_string(value__0 string) struct{} {
    var t29 struct{} = string_print(value__0)
    return t29
}

func println__T_int32(value__1 int32) struct{} {
    var t30 string = int32_to_string(value__1)
    var t31 struct{} = string_println(t30)
    return t31
}

func main() {
    main0()
}
