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
    var i__1 *ref_int32_x = ref__Ref_int32(1)
    for {
        var t27 int32 = ref_get__Ref_int32(i__1)
        var t28 bool = t27 <= 100
        if !t28 {
            break
        }
        var t35 int32 = ref_get__Ref_int32(i__1)
        var t36 bool = t35 == 50
        if t36 {
            break
        } else {
            var t30 int32 = ref_get__Ref_int32(sum__0)
            var t31 int32 = ref_get__Ref_int32(i__1)
            var t32 int32 = t30 + t31
            ref_set__Ref_int32(sum__0, t32)
            var t33 int32 = ref_get__Ref_int32(i__1)
            var t34 int32 = t33 + 1
            ref_set__Ref_int32(i__1, t34)
            continue
        }
    }
    print__T_string("sum up to break: ")
    var t13 int32 = ref_get__Ref_int32(sum__0)
    println__T_int32(t13)
    var even_sum__2 *ref_int32_x = ref__Ref_int32(0)
    var j__3 *ref_int32_x = ref__Ref_int32(1)
    for {
        var t17 int32 = ref_get__Ref_int32(j__3)
        var t18 bool = t17 <= 10
        if !t18 {
            break
        }
        var cur__4 int32 = ref_get__Ref_int32(j__3)
        var t19 int32 = cur__4 + 1
        ref_set__Ref_int32(j__3, t19)
        var t21 int32 = cur__4 / 2
        var t22 int32 = t21 * 2
        var t23 bool = cur__4 == t22
        if t23 {
            var t24 int32 = ref_get__Ref_int32(even_sum__2)
            var t25 int32 = t24 + cur__4
            ref_set__Ref_int32(even_sum__2, t25)
            continue
        } else {
            continue
        }
    }
    print__T_string("even sum: ")
    var t15 int32 = ref_get__Ref_int32(even_sum__2)
    println__T_int32(t15)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t37 struct{} = string_print(value__0)
    return t37
}

func println__T_int32(value__1 int32) struct{} {
    var t38 string = int32_to_string(value__1)
    var t39 struct{} = string_println(t38)
    return t39
}

func main() {
    main0()
}
