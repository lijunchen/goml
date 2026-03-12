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

type Mode interface {
    isMode()
}

type Take struct {}

func (_ Take) isMode() {}

type Skip struct {}

func (_ Skip) isMode() {}

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

func maybe_num__native(flag__0 bool) (int32, bool) {
    if flag__0 {
        return 8, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func nested__native(top__1 bool, mode__2 Mode, inner_flag__3 bool) (int32, bool) {
    var jp16 int32
    if top__1 {
        var jp19 int32
        switch mode__2.(type) {
        case Take:
            var jp21 int32
            var mtmp0_value_0 int32
            var mtmp0_ok bool
            mtmp0_value_0, mtmp0_ok = maybe_num__native(inner_flag__3)
            if !mtmp0_ok {
                var ret_zero int32
                return ret_zero, false
            }
            jp21 = mtmp0_value_0
            var inner__4 int32 = jp21
            var t22 int32 = inner__4 + 1
            jp19 = t22
            jp16 = jp19
            var value__6 int32 = jp16
            return value__6, true
        case Skip:
            jp19 = 20
            jp16 = jp19
            var value__6 int32 = jp16
            return value__6, true
        default:
            panic("non-exhaustive match")
        }
    } else {
        var jp24 int32
        var mtmp2_value_0 int32
        var mtmp2_ok bool
        mtmp2_value_0, mtmp2_ok = maybe_num__native(inner_flag__3)
        if !mtmp2_ok {
            var ret_zero int32
            return ret_zero, false
        }
        jp24 = mtmp2_value_0
        var inner__5 int32 = jp24
        var t25 int32 = inner__5 + 2
        jp16 = t25
        var value__6 int32 = jp16
        return value__6, true
    }
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = nested__native(top__1, mode__2, inner_flag__3)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
}

func show(opt__7 Option__int32) string {
    var retv27 string
    var jp29 string
    switch opt__7.(type) {
    case None:
        jp29 = "none"
    case Some:
        var x4 int32 = opt__7.(Some)._0
        var value__8 int32 = x4
        var t30 string = int32_to_string(value__8)
        var t31 string = "some=" + t30
        jp29 = t31
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    var t33 Option__int32 = nested(true, Take{}, true)
    var t34 string = show(t33)
    println__T_string(t34)
    var t35 Option__int32 = nested(true, Skip{}, false)
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Option__int32 = nested(false, Take{}, false)
    var t38 string = show(t37)
    println__T_string(t38)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
