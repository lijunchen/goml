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

func main0() struct{} {
    var sum__0 *ref_int32_x = ref__Ref_5int32(0)
    var i__1 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop12:
    for {
        var t13 int32 = ref_get__Ref_5int32(i__1)
        var t14 bool = t13 < 20
        if t14 {
            var t15 int32 = ref_get__Ref_5int32(i__1)
            var t16 int32 = t15 + 1
            ref_set__Ref_5int32(i__1, t16)
            var t21 int32 = ref_get__Ref_5int32(i__1)
            var t22 bool = t21 > 5
            if t22 {
                break Loop_loop12
            } else {
                var t18 int32 = ref_get__Ref_5int32(sum__0)
                var t19 int32 = ref_get__Ref_5int32(i__1)
                var t20 int32 = t18 + t19
                ref_set__Ref_5int32(sum__0, t20)
                continue
            }
        } else {
            break Loop_loop12
        }
    }
    print__T_string("sum: ")
    var t10 int32 = ref_get__Ref_5int32(sum__0)
    println__T_int32(t10)
    print__T_string("i at break: ")
    var t11 int32 = ref_get__Ref_5int32(i__1)
    println__T_int32(t11)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t26 string = int32_to_string(value__1)
    string_println(t26)
    return struct{}{}
}

func main() {
    main0()
}
