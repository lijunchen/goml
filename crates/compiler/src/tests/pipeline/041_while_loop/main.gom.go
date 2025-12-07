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
    var ret21 int32
    var acc__1 *ref_int32_x = ref__Ref_int32(0)
    var i__2 *ref_int32_x = ref__Ref_int32(0)
    var cond22 bool
    for {
        var t8 int32 = ref_get__Ref_int32(i__2)
        cond22 = t8 < limit__0
        if !cond22 {
            break
        }
        var current__3 int32 = ref_get__Ref_int32(i__2)
        var t10 int32 = ref_get__Ref_int32(acc__1)
        var t9 int32 = t10 + current__3
        ref_set__Ref_int32(acc__1, t9)
        var t11 int32 = current__3 + 1
        ref_set__Ref_int32(i__2, t11)
    }
    ret21 = ref_get__Ref_int32(acc__1)
    return ret21
}

func sum_even(limit__4 int32) int32 {
    var ret23 int32
    var acc__5 *ref_int32_x = ref__Ref_int32(0)
    var i__6 *ref_int32_x = ref__Ref_int32(0)
    var is_even__7 *ref_bool_x = ref__Ref_bool(true)
    var cond24 bool
    for {
        var t12 int32 = ref_get__Ref_int32(i__6)
        cond24 = t12 < limit__4
        if !cond24 {
            break
        }
        var current__8 int32 = ref_get__Ref_int32(i__6)
        var t13 int32 = current__8 + 1
        ref_set__Ref_int32(i__6, t13)
        var add_now__9 bool = ref_get__Ref_bool(is_even__7)
        var t14 bool = !add_now__9
        ref_set__Ref_bool(is_even__7, t14)
        if add_now__9 {
            var t16 int32 = ref_get__Ref_int32(acc__5)
            var t15 int32 = t16 + current__8
            ref_set__Ref_int32(acc__5, t15)
        } else {}
    }
    ret23 = ref_get__Ref_int32(acc__5)
    return ret23
}

func main0() struct{} {
    var ret25 struct{}
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var t18 string = int32_to_string(first__10)
    var t17 string = "sum_to(5)=" + t18
    string_println(t17)
    var t20 string = int32_to_string(evens__11)
    var t19 string = "sum_even(6)=" + t20
    string_println(t19)
    ret25 = struct{}{}
    return ret25
}

func main() {
    main0()
}
