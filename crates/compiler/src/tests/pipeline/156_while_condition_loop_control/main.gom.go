package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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
    Loop_loop29:
    for {
        var t40 int32 = ref_get__Ref_int32(i__0)
        var t41 bool = t40 == 0
        var jp31 bool
        if t41 {
            ref_set__Ref_int32(i__0, 1)
            jp31 = true
        } else {
            var t44 int32 = ref_get__Ref_int32(i__0)
            var t45 bool = t44 < 4
            var jp43 bool
            if t45 {
                jp43 = true
            } else {
                jp43 = false
            }
            jp31 = jp43
        }
        if jp31 {
            var t32 int32 = ref_get__Ref_int32(total__1)
            var t33 int32 = ref_get__Ref_int32(i__0)
            var t34 int32 = t32 + t33
            ref_set__Ref_int32(total__1, t34)
            var t38 int32 = ref_get__Ref_int32(i__0)
            var t39 bool = t38 == 1
            if t39 {
                ref_set__Ref_int32(i__0, 2)
                continue
            } else {
                var t36 int32 = ref_get__Ref_int32(i__0)
                var t37 int32 = t36 + 1
                ref_set__Ref_int32(i__0, t37)
                continue
            }
        } else {
            break Loop_loop29
        }
    }
    var t17 int32 = ref_get__Ref_int32(total__1)
    println__T_int32(t17)
    var j__2 *ref_int32_x = ref__Ref_int32(0)
    var total2__3 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop20:
    for {
        var mtmp7 int32 = ref_get__Ref_int32(j__2)
        var jp22 bool
        switch mtmp7 {
        case 0:
            ref_set__Ref_int32(j__2, 1)
            jp22 = true
        case 1:
            ref_set__Ref_int32(j__2, 2)
            jp22 = true
        case 2:
            jp22 = true
        default:
            jp22 = false
        }
        if jp22 {
            var t23 int32 = ref_get__Ref_int32(total2__3)
            var t24 int32 = ref_get__Ref_int32(j__2)
            var t25 int32 = t23 + t24
            ref_set__Ref_int32(total2__3, t25)
            var t27 int32 = ref_get__Ref_int32(j__2)
            var t28 bool = t27 == 2
            if t28 {
                break Loop_loop20
            } else {
                continue
            }
        } else {
            break Loop_loop20
        }
    }
    var t19 int32 = ref_get__Ref_int32(total2__3)
    println__T_int32(t19)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t47 string = int32_to_string(value__1)
    string_println(t47)
    return struct{}{}
}

func main() {
    main0()
}
