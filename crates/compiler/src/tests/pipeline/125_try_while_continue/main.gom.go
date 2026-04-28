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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type GoError = error

func step__native(i__0 int32) (int32, bool) {
    var t13 bool = i__0 == 2
    if t13 {
        var ret_zero int32
        return ret_zero, false
    } else {
        var t14 int32 = i__0 + 10
        return t14, true
    }
}

func accumulate__native(limit__1 int32) (int32, bool) {
    var sum__2 *ref_int32_x = ref__Ref_5int32(0)
    var i__3 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop21:
    for {
        var t22 int32 = ref_get__Ref_5int32(i__3)
        var t23 bool = t22 < limit__1
        if t23 {
            var cur__4 int32 = ref_get__Ref_5int32(i__3)
            var t24 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t24)
            var t30 bool = cur__4 == 1
            if t30 {
                continue
            } else {
                var jp27 int32
                var mtmp2_value_0 int32
                var mtmp2_ok bool
                mtmp2_value_0, mtmp2_ok = step__native(cur__4)
                if !mtmp2_ok {
                    var ret_zero int32
                    return ret_zero, false
                }
                jp27 = mtmp2_value_0
                var value__5 int32 = jp27
                var t28 int32 = ref_get__Ref_5int32(sum__2)
                var t29 int32 = t28 + value__5
                ref_set__Ref_5int32(sum__2, t29)
                continue
            }
        } else {
            break Loop_loop21
        }
    }
    var t19 int32 = ref_get__Ref_5int32(sum__2)
    return t19, true
}

func accumulate(limit__1 int32) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = accumulate__native(limit__1)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
}

func show(opt__6 Option__int32) string {
    var retv32 string
    var jp34 string
    switch opt__6.(type) {
    case None:
        jp34 = "none"
    case Some:
        var x6 int32 = opt__6.(Some)._0
        var value__7 int32 = x6
        var t35 string = int32_to_string(value__7)
        var t36 string = "some=" + t35
        jp34 = t36
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var t38 Option__int32 = accumulate(2)
    var t39 string = show(t38)
    println__T_string(t39)
    var t40 Option__int32 = accumulate(4)
    var t41 string = show(t40)
    println__T_string(t41)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
