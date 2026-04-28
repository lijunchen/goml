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
    Loop_loop46:
    for {
        var t52 int32 = ref_get__Ref_int32(i__0)
        var t53 bool = t52 < 3
        var jp48 bool
        if t53 {
            jp48 = true
        } else {
            jp48 = false
        }
        if jp48 {
            var t49 int32 = ref_get__Ref_int32(i__0)
            println__T_int32(t49)
            var t50 int32 = ref_get__Ref_int32(i__0)
            var t51 int32 = t50 + 1
            ref_set__Ref_int32(i__0, t51)
            continue
        } else {
            break Loop_loop46
        }
    }
    var j__1 *ref_int32_x = ref__Ref_int32(0)
    var total__2 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop30:
    for {
        var t38 int32 = ref_get__Ref_int32(j__1)
        var t39 bool = t38 < 4
        var jp32 bool
        if t39 {
            var t42 int32 = ref_get__Ref_int32(j__1)
            var t43 bool = t42 == 1
            var jp41 bool
            if t43 {
                jp41 = true
            } else {
                var t44 int32 = ref_get__Ref_int32(j__1)
                var t45 bool = t44 != 3
                jp41 = t45
            }
            jp32 = jp41
        } else {
            jp32 = false
        }
        if jp32 {
            var t33 int32 = ref_get__Ref_int32(total__2)
            var t34 int32 = ref_get__Ref_int32(j__1)
            var t35 int32 = t33 + t34
            ref_set__Ref_int32(total__2, t35)
            var t36 int32 = ref_get__Ref_int32(j__1)
            var t37 int32 = t36 + 1
            ref_set__Ref_int32(j__1, t37)
            continue
        } else {
            break Loop_loop30
        }
    }
    var t15 int32 = ref_get__Ref_int32(total__2)
    println__T_int32(t15)
    var k__3 *ref_int32_x = ref__Ref_int32(0)
    var sum__4 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop18:
    for {
        var mtmp7 int32 = ref_get__Ref_int32(k__3)
        var jp20 bool
        switch mtmp7 {
        case 0:
            jp20 = true
        case 1:
            var t28 int32 = ref_get__Ref_int32(sum__4)
            var t29 bool = t28 == 0
            var jp27 bool
            if t29 {
                jp27 = true
            } else {
                jp27 = false
            }
            jp20 = jp27
        case 2:
            jp20 = true
        default:
            jp20 = false
        }
        if jp20 {
            var t21 int32 = ref_get__Ref_int32(sum__4)
            var t22 int32 = ref_get__Ref_int32(k__3)
            var t23 int32 = t21 + t22
            ref_set__Ref_int32(sum__4, t23)
            var t24 int32 = ref_get__Ref_int32(k__3)
            var t25 int32 = t24 + 1
            ref_set__Ref_int32(k__3, t25)
            continue
        } else {
            break Loop_loop18
        }
    }
    var t17 int32 = ref_get__Ref_int32(sum__4)
    println__T_int32(t17)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t55 string = int32_to_string(value__1)
    string_println(t55)
    return struct{}{}
}

func main() {
    main0()
}
