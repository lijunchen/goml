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
    var retv11 int32
    var acc__1 *ref_int32_x = ref__Ref_int32(0)
    var i__2 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t15 int32 = ref_get__Ref_int32(i__2)
        var t16 bool = t15 < limit__0
        if !t16 {
            break
        }
        var current__3 int32 = ref_get__Ref_int32(i__2)
        var t17 int32 = ref_get__Ref_int32(acc__1)
        var t18 int32 = t17 + current__3
        ref_set__Ref_int32(acc__1, t18)
        var t19 int32 = current__3 + 1
        ref_set__Ref_int32(i__2, t19)
        continue
    }
    var t13 int32 = ref_get__Ref_int32(acc__1)
    retv11 = t13
    return retv11
}

func sum_even(limit__4 int32) int32 {
    var retv21 int32
    var acc__5 *ref_int32_x = ref__Ref_int32(0)
    var i__6 *ref_int32_x = ref__Ref_int32(0)
    var is_even__7 *ref_bool_x = ref__Ref_bool(true)
    for {
        var t25 int32 = ref_get__Ref_int32(i__6)
        var t26 bool = t25 < limit__4
        if !t26 {
            break
        }
        var current__8 int32 = ref_get__Ref_int32(i__6)
        var t27 int32 = current__8 + 1
        ref_set__Ref_int32(i__6, t27)
        var add_now__9 bool = ref_get__Ref_bool(is_even__7)
        var t28 bool = !add_now__9
        ref_set__Ref_bool(is_even__7, t28)
        if add_now__9 {
            var t30 int32 = ref_get__Ref_int32(acc__5)
            var t31 int32 = t30 + current__8
            ref_set__Ref_int32(acc__5, t31)
        } else {}
        continue
    }
    var t23 int32 = ref_get__Ref_int32(acc__5)
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

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t37 string = int32_to_string(value__1)
    string_println(t37)
    return struct{}{}
}

func main() {
    main0()
}
