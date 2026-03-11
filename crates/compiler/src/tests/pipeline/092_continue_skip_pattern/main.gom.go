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
    for {
        var t9 int32 = ref_get__Ref_int32(i__0)
        var t10 bool = t9 < 8
        if !t10 {
            break
        }
        var t11 int32 = ref_get__Ref_int32(i__0)
        var t12 int32 = t11 + 1
        ref_set__Ref_int32(i__0, t12)
        var t18 int32 = ref_get__Ref_int32(i__0)
        var t19 bool = t18 == 3
        if t19 {
            continue
        } else {
            var t16 int32 = ref_get__Ref_int32(i__0)
            var t17 bool = t16 == 6
            if t17 {
                continue
            } else {
                var t15 int32 = ref_get__Ref_int32(i__0)
                println__T_int32(t15)
                continue
            }
        }
    }
    println__T_string("done")
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t21 string = int32_to_string(value__1)
    string_println(t21)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
