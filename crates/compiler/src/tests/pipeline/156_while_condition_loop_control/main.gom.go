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

type GoError = error

func main0() struct{} {
    var i__0 *ref_int32_x = ref__Ref_int32(0)
    var total__1 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop22:
    for {
        var t30 int32 = ref_get__Ref_int32(i__0)
        var t31 bool = t30 == 0
        var jp24 bool
        if t31 {
            ref_set__Ref_int32(i__0, 1)
            continue
        } else {
            var t34 int32 = ref_get__Ref_int32(i__0)
            var t35 bool = t34 < 4
            var jp33 bool
            if t35 {
                jp33 = true
            } else {
                jp33 = false
            }
            jp24 = jp33
            if jp24 {
                var t25 int32 = ref_get__Ref_int32(total__1)
                var t26 int32 = ref_get__Ref_int32(i__0)
                var t27 int32 = t25 + t26
                ref_set__Ref_int32(total__1, t27)
                var t28 int32 = ref_get__Ref_int32(i__0)
                var t29 int32 = t28 + 1
                ref_set__Ref_int32(i__0, t29)
                continue
            } else {
                break Loop_loop22
            }
        }
    }
    var t13 int32 = ref_get__Ref_int32(total__1)
    println__T_int32(t13)
    var j__2 *ref_int32_x = ref__Ref_int32(0)
    var total2__3 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop16:
    for {
        var mtmp5 int32 = ref_get__Ref_int32(j__2)
        var jp18 bool
        switch mtmp5 {
        case 0:
            ref_set__Ref_int32(j__2, 1)
            jp18 = true
            if jp18 {
                var t19 int32 = ref_get__Ref_int32(total2__3)
                var t20 int32 = ref_get__Ref_int32(j__2)
                var t21 int32 = t19 + t20
                ref_set__Ref_int32(total2__3, t21)
                continue
            } else {
                break Loop_loop16
            }
        case 1:
            ref_set__Ref_int32(j__2, 2)
            jp18 = true
            if jp18 {
                var t19 int32 = ref_get__Ref_int32(total2__3)
                var t20 int32 = ref_get__Ref_int32(j__2)
                var t21 int32 = t19 + t20
                ref_set__Ref_int32(total2__3, t21)
                continue
            } else {
                break Loop_loop16
            }
        default:
            break Loop_loop16
        }
    }
    var t15 int32 = ref_get__Ref_int32(total2__3)
    println__T_int32(t15)
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
