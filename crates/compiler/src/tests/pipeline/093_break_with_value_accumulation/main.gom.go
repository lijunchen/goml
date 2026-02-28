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

func main0() struct{} {
    var sum__0 *ref_int32_x = ref__Ref_int32(0)
    var i__1 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop11:
    for {
        var t12 int32 = ref_get__Ref_int32(i__1)
        var t13 bool = t12 < 20
        if !t13 {
            break
        }
        var t14 int32 = ref_get__Ref_int32(i__1)
        var t15 int32 = t14 + 1
        ref_set__Ref_int32(i__1, t15)
        var t20 int32 = ref_get__Ref_int32(i__1)
        var t21 bool = t20 > 5
        if t21 {
            break Loop_loop11
        } else {
            var t17 int32 = ref_get__Ref_int32(sum__0)
            var t18 int32 = ref_get__Ref_int32(i__1)
            var t19 int32 = t17 + t18
            ref_set__Ref_int32(sum__0, t19)
            continue
        }
    }
    print__T_string("sum: ")
    var t9 int32 = ref_get__Ref_int32(sum__0)
    println__T_int32(t9)
    print__T_string("i at break: ")
    var t10 int32 = ref_get__Ref_int32(i__1)
    println__T_int32(t10)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t22 struct{} = string_print(value__0)
    return t22
}

func println__T_int32(value__1 int32) struct{} {
    var t23 string = int32_to_string(value__1)
    var t24 struct{} = string_println(t23)
    return t24
}

func main() {
    main0()
}
