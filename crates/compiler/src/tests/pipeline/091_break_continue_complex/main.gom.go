package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var sum__0 *ref_int32_x = ref__Ref_5int32(0)
    var i__1 *ref_int32_x = ref__Ref_5int32(1)
    Loop_loop27:
    for {
        var t28 int32 = ref_get__Ref_5int32(i__1)
        var t29 bool = t28 <= 100
        if t29 {
            var t36 int32 = ref_get__Ref_5int32(i__1)
            var t37 bool = t36 == 50
            if t37 {
                break Loop_loop27
            } else {
                var t31 int32 = ref_get__Ref_5int32(sum__0)
                var t32 int32 = ref_get__Ref_5int32(i__1)
                var t33 int32 = t31 + t32
                ref_set__Ref_5int32(sum__0, t33)
                var t34 int32 = ref_get__Ref_5int32(i__1)
                var t35 int32 = t34 + 1
                ref_set__Ref_5int32(i__1, t35)
                continue
            }
        } else {
            break Loop_loop27
        }
    }
    print__T_string("sum up to break: ")
    var t14 int32 = ref_get__Ref_5int32(sum__0)
    println__T_int32(t14)
    var even_sum__2 *ref_int32_x = ref__Ref_5int32(0)
    var j__3 *ref_int32_x = ref__Ref_5int32(1)
    Loop_loop17:
    for {
        var t18 int32 = ref_get__Ref_5int32(j__3)
        var t19 bool = t18 <= 10
        if t19 {
            var cur__4 int32 = ref_get__Ref_5int32(j__3)
            var t20 int32 = cur__4 + 1
            ref_set__Ref_5int32(j__3, t20)
            var t22 int32 = cur__4 / 2
            var t23 int32 = t22 * 2
            var t24 bool = cur__4 == t23
            if t24 {
                var t25 int32 = ref_get__Ref_5int32(even_sum__2)
                var t26 int32 = t25 + cur__4
                ref_set__Ref_5int32(even_sum__2, t26)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop17
        }
    }
    print__T_string("even sum: ")
    var t16 int32 = ref_get__Ref_5int32(even_sum__2)
    println__T_int32(t16)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t41 string = int32_to_string(value__1)
    string_println(t41)
    return struct{}{}
}

func main() {
    main0()
}
