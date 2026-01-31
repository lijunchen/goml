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
    var ret20 int32
    var acc__1 *ref_int32_x = ref__Ref_int32(0)
    var i__2 *ref_int32_x = ref__Ref_int32(0)
    var cond21 bool
    for {
        var t10 int32 = ref_get__Ref_int32(i__2)
        cond21 = t10 < limit__0
        if !cond21 {
            break
        }
        var current__3 int32 = ref_get__Ref_int32(i__2)
        var t12 int32 = ref_get__Ref_int32(acc__1)
        var t11 int32 = t12 + current__3
        ref_set__Ref_int32(acc__1, t11)
        var t13 int32 = current__3 + 1
        ref_set__Ref_int32(i__2, t13)
    }
    ret20 = ref_get__Ref_int32(acc__1)
    return ret20
}

func sum_even(limit__4 int32) int32 {
    var ret22 int32
    var acc__5 *ref_int32_x = ref__Ref_int32(0)
    var i__6 *ref_int32_x = ref__Ref_int32(0)
    var is_even__7 *ref_bool_x = ref__Ref_bool(true)
    var cond23 bool
    for {
        var t14 int32 = ref_get__Ref_int32(i__6)
        cond23 = t14 < limit__4
        if !cond23 {
            break
        }
        var current__8 int32 = ref_get__Ref_int32(i__6)
        var t15 int32 = current__8 + 1
        ref_set__Ref_int32(i__6, t15)
        var add_now__9 bool = ref_get__Ref_bool(is_even__7)
        var t16 bool = !add_now__9
        ref_set__Ref_bool(is_even__7, t16)
        if add_now__9 {
            var t18 int32 = ref_get__Ref_int32(acc__5)
            var t17 int32 = t18 + current__8
            ref_set__Ref_int32(acc__5, t17)
        } else {}
    }
    ret22 = ref_get__Ref_int32(acc__5)
    return ret22
}

func main0() struct{} {
    var ret24 struct{}
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    print__T_string("sum_to(5)=")
    println__T_int32(first__10)
    print__T_string("sum_even(6)=")
    println__T_int32(evens__11)
    ret24 = struct{}{}
    return ret24
}

func print__T_string(value__0 string) struct{} {
    var ret25 struct{}
    ret25 = string_print(value__0)
    return ret25
}

func println__T_int32(value__1 int32) struct{} {
    var ret26 struct{}
    var t19 string = int32_to_string(value__1)
    ret26 = string_println(t19)
    return ret26
}

func main() {
    main0()
}
