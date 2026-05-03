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

func step_some(i__0 int32) Option__bool {
    var retv14 Option__bool
    var t17 bool = i__0 < 3
    var jp16 Option__bool
    if t17 {
        var t18 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp16 = t18
    } else {
        var t19 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp16 = t19
    }
    retv14 = jp16
    return retv14
}

func step_none(i__1 int32) Option__bool {
    var retv21 Option__bool
    var t24 bool = i__1 < 2
    var jp23 Option__bool
    if t24 {
        var t25 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp23 = t25
    } else {
        jp23 = Option__bool_None{}
    }
    retv21 = jp23
    return retv21
}

func run_some() Option__int32 {
    var retv27 Option__int32
    var i__2 *ref_int32_x = ref__Ref_5int32(0)
    var total__3 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop31:
    for {
        var t32 int32 = ref_get__Ref_5int32(i__2)
        var mtmp0 Option__bool = step_some(t32)
        var jp34 bool
        switch mtmp0.(type) {
        case Option__bool_None:
            retv27 = Option__int32_None{}
            return retv27
        case Option__bool_Some:
            var x1 bool = mtmp0.(Option__bool_Some)._0
            var try_value__31 bool = x1
            jp34 = try_value__31
            if jp34 {
                var t35 int32 = ref_get__Ref_5int32(total__3)
                var t36 int32 = ref_get__Ref_5int32(i__2)
                var t37 int32 = t35 + t36
                ref_set__Ref_5int32(total__3, t37)
                var t38 int32 = ref_get__Ref_5int32(i__2)
                var t39 int32 = t38 + 1
                ref_set__Ref_5int32(i__2, t39)
                continue
            } else {
                break Loop_loop31
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t29 int32 = ref_get__Ref_5int32(total__3)
    var t30 Option__int32 = Option__int32_Some{
        _0: t29,
    }
    retv27 = t30
    return retv27
}

func run_none() Option__int32 {
    var retv41 Option__int32
    var i__4 *ref_int32_x = ref__Ref_5int32(0)
    var total__5 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop45:
    for {
        var t46 int32 = ref_get__Ref_5int32(i__4)
        var mtmp5 Option__bool = step_none(t46)
        var jp48 bool
        switch mtmp5.(type) {
        case Option__bool_None:
            retv41 = Option__int32_None{}
            return retv41
        case Option__bool_Some:
            var x6 bool = mtmp5.(Option__bool_Some)._0
            var try_value__67 bool = x6
            jp48 = try_value__67
            if jp48 {
                var t49 int32 = ref_get__Ref_5int32(total__5)
                var t50 int32 = ref_get__Ref_5int32(i__4)
                var t51 int32 = t49 + t50
                ref_set__Ref_5int32(total__5, t51)
                var t52 int32 = ref_get__Ref_5int32(i__4)
                var t53 int32 = t52 + 1
                ref_set__Ref_5int32(i__4, t53)
                continue
            } else {
                break Loop_loop45
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t43 int32 = ref_get__Ref_5int32(total__5)
    var t44 Option__int32 = Option__int32_Some{
        _0: t43,
    }
    retv41 = t44
    return retv41
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
