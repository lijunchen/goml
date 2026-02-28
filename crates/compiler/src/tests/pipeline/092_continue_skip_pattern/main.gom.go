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

func main0() struct{} {
    var i__0 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t8 int32 = ref_get__Ref_int32(i__0)
        var t9 bool = t8 < 8
        if !t9 {
            break
        }
        var t10 int32 = ref_get__Ref_int32(i__0)
        var t11 int32 = t10 + 1
        ref_set__Ref_int32(i__0, t11)
        var t17 int32 = ref_get__Ref_int32(i__0)
        var t18 bool = t17 == 3
        if t18 {
            continue
        } else {
            var t15 int32 = ref_get__Ref_int32(i__0)
            var t16 bool = t15 == 6
            if t16 {
                continue
            } else {
                var t14 int32 = ref_get__Ref_int32(i__0)
                println__T_int32(t14)
                continue
            }
        }
    }
    println__T_string("done")
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t19 string = int32_to_string(value__1)
    var t20 struct{} = string_println(t19)
    return t20
}

func println__T_string(value__1 string) struct{} {
    var t21 struct{} = string_println(value__1)
    return t21
}

func main() {
    main0()
}
