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
    var t11 int32
    var t13 int32
    var t14 bool
    var current__3 int32
    var t15 int32
    var t16 int32
    var t17 int32
    acc__1 = ref__Ref_int32(0)
    i__2 = ref__Ref_int32(0)
    goto b2
    b1:
    t11 = ref_get__Ref_int32(acc__1)
    return t11
    b2:
    t13 = ref_get__Ref_int32(i__2)
    t14 = t13 < limit__0
    if t14 {
        goto b3
    } else {
        goto b4
    }
    b3:
    current__3 = ref_get__Ref_int32(i__2)
    t15 = ref_get__Ref_int32(acc__1)
    t16 = t15 + current__3
    ref_set__Ref_int32(acc__1, t16)
    t17 = current__3 + 1
    ref_set__Ref_int32(i__2, t17)
    goto b2
    b4:
    goto b1
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var i__6 *ref_int32_x
    var is_even__7 *ref_bool_x
    var t19 int32
    var t21 int32
    var t22 bool
    var current__8 int32
    var t23 int32
    var add_now__9 bool
    var t24 bool
    var t26 int32
    var t27 int32
    acc__5 = ref__Ref_int32(0)
    i__6 = ref__Ref_int32(0)
    is_even__7 = ref__Ref_bool(true)
    goto b2
    b1:
    t19 = ref_get__Ref_int32(acc__5)
    return t19
    b2:
    t21 = ref_get__Ref_int32(i__6)
    t22 = t21 < limit__4
    if t22 {
        goto b3
    } else {
        goto b7
    }
    b3:
    current__8 = ref_get__Ref_int32(i__6)
    t23 = current__8 + 1
    ref_set__Ref_int32(i__6, t23)
    add_now__9 = ref_get__Ref_bool(is_even__7)
    t24 = !add_now__9
    ref_set__Ref_bool(is_even__7, t24)
    if add_now__9 {
        goto b5
    } else {
        goto b6
    }
    b4:
    goto b2
    b5:
    t26 = ref_get__Ref_int32(acc__5)
    t27 = t26 + current__8
    ref_set__Ref_int32(acc__5, t27)
    goto b4
    b6:
    goto b4
    b7:
    goto b1
}

func main0() struct{} {
    var first__10 int32
    var evens__11 int32
    first__10 = sum_to(5)
    evens__11 = sum_even(6)
    print__T_string("sum_to(5)=")
    println__T_int32(first__10)
    print__T_string("sum_even(6)=")
    println__T_int32(evens__11)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t29 struct{}
    t29 = string_print(value__0)
    return t29
}

func println__T_int32(value__1 int32) struct{} {
    var t30 string
    var t31 struct{}
    t30 = int32_to_string(value__1)
    t31 = string_println(t30)
    return t31
}

func main() {
    main0()
}
