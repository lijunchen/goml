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

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type GoError = error

func step_some__native(i__0 int32) (bool, bool) {
    var t17 bool = i__0 < 3
    if t17 {
        return true, true
    } else {
        return false, true
    }
}

func step_none__native(i__1 int32) (bool, bool) {
    var t24 bool = i__1 < 2
    if t24 {
        return true, true
    } else {
        var ret_zero bool
        return ret_zero, false
    }
}

func run_some__native() (int32, bool) {
    var i__2 *ref_int32_x = ref__Ref_int32(0)
    var total__3 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop31:
    for {
        var t32 int32 = ref_get__Ref_int32(i__2)
        var jp34 bool
        var mtmp0_value_0 bool
        var mtmp0_ok bool
        mtmp0_value_0, mtmp0_ok = step_some__native(t32)
        if !mtmp0_ok {
            var ret_zero int32
            return ret_zero, false
        }
        jp34 = mtmp0_value_0
        if jp34 {
            var t35 int32 = ref_get__Ref_int32(total__3)
            var t36 int32 = ref_get__Ref_int32(i__2)
            var t37 int32 = t35 + t36
            ref_set__Ref_int32(total__3, t37)
            var t38 int32 = ref_get__Ref_int32(i__2)
            var t39 int32 = t38 + 1
            ref_set__Ref_int32(i__2, t39)
            continue
        } else {
            break Loop_loop31
        }
    }
    var t29 int32 = ref_get__Ref_int32(total__3)
    return t29, true
}

func run_some() Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = run_some__native()
    if native_ok {
        return Option__int32_Some{
            _0: native_value_0,
        }
    }
    return Option__int32_None{}
}

func run_none__native() (int32, bool) {
    var i__4 *ref_int32_x = ref__Ref_int32(0)
    var total__5 *ref_int32_x = ref__Ref_int32(0)
    Loop_loop45:
    for {
        var t46 int32 = ref_get__Ref_int32(i__4)
        var jp48 bool
        var mtmp5_value_0 bool
        var mtmp5_ok bool
        mtmp5_value_0, mtmp5_ok = step_none__native(t46)
        if !mtmp5_ok {
            var ret_zero int32
            return ret_zero, false
        }
        jp48 = mtmp5_value_0
        if jp48 {
            var t49 int32 = ref_get__Ref_int32(total__5)
            var t50 int32 = ref_get__Ref_int32(i__4)
            var t51 int32 = t49 + t50
            ref_set__Ref_int32(total__5, t51)
            var t52 int32 = ref_get__Ref_int32(i__4)
            var t53 int32 = t52 + 1
            ref_set__Ref_int32(i__4, t53)
            continue
        } else {
            break Loop_loop45
        }
    }
    var t43 int32 = ref_get__Ref_int32(total__5)
    return t43, true
}

func run_none() Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = run_none__native()
    if native_ok {
        return Option__int32_Some{
            _0: native_value_0,
        }
    }
    return Option__int32_None{}
}

func show(x__6 Option__int32) string {
    var retv55 string
    var jp57 string
    switch x__6.(type) {
    case Option__int32_None:
        jp57 = "none"
    case Option__int32_Some:
        var x10 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x10
        var t58 string = int32_to_string(v__7)
        var t59 string = "some=" + t58
        jp57 = t59
    default:
        panic("non-exhaustive match")
    }
    retv55 = jp57
    return retv55
}

func main0() struct{} {
    var t61 Option__int32 = run_some()
    var t62 string = show(t61)
    println__T_string(t62)
    var t63 Option__int32 = run_none()
    var t64 string = show(t63)
    println__T_string(t64)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
