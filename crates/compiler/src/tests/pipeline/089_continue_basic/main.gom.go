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
        var t7 int32 = ref_get__Ref_int32(i__0)
        var t8 bool = t7 < 10
        if !t8 {
            break
        }
        var t9 int32 = ref_get__Ref_int32(i__0)
        var t10 int32 = t9 + 1
        ref_set__Ref_int32(i__0, t10)
        var t13 int32 = ref_get__Ref_int32(i__0)
        var t14 bool = t13 == 5
        if t14 {
            continue
        } else {
            var t12 int32 = ref_get__Ref_int32(i__0)
            println__T_int32(t12)
            continue
        }
    }
    println__T_string("done")
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t15 string = int32_to_string(value__1)
    var t16 struct{} = string_println(t15)
    return t16
}

func println__T_string(value__1 string) struct{} {
    var t17 struct{} = string_println(value__1)
    return t17
}

func main() {
    main0()
}
