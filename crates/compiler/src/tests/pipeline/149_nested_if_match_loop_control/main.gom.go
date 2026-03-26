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
    var sum__1 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop8:
    for {
        var t9 int32 = ref_get__Ref_int32(i__0)
        var t10 bool = t9 < 7
        if t10 {
            var cur__2 int32 = ref_get__Ref_int32(i__0)
            var t11 int32 = cur__2 + 1
            ref_set__Ref_int32(i__0, t11)
            var t15 bool = cur__2 < 5
            if t15 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t13 int32 = ref_get__Ref_int32(sum__1)
                    var t14 int32 = t13 + cur__2
                    ref_set__Ref_int32(sum__1, t14)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop8
                default:
                    var t13 int32 = ref_get__Ref_int32(sum__1)
                    var t14 int32 = t13 + cur__2
                    ref_set__Ref_int32(sum__1, t14)
                    continue
                }
            }
        } else {
            break Loop_loop8
        }
    }
    var t7 int32 = ref_get__Ref_int32(sum__1)
    println__T_int32(t7)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t19 string = int32_to_string(value__1)
    string_println(t19)
    return struct{}{}
}

func main() {
    main0()
}
