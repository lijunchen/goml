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

func maybe_seed__native(flag__0 bool) (int32, bool) {
    if flag__0 {
        return 3, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func maybe_double__native(value__1 int32) (int32, bool) {
    var t16 bool = value__1 > 0
    if t16 {
        var t17 int32 = value__1 * 2
        return t17, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func maybe_total__native(flag__2 bool) (int32, bool) {
    var jp22 int32
    var mtmp0_value int32
    var mtmp0_ok bool
    mtmp0_value, mtmp0_ok = maybe_seed__native(flag__2)
    if !mtmp0_ok {
        var ret_zero int32
        return ret_zero, false
    }
    jp22 = mtmp0_value
    var a__3 int32 = jp22
    var jp24 int32
    var mtmp2_value int32
    var mtmp2_ok bool
    mtmp2_value, mtmp2_ok = maybe_double__native(a__3)
    if !mtmp2_ok {
        var ret_zero int32
        return ret_zero, false
    }
    jp24 = mtmp2_value
    var b__4 int32 = jp24
    var t25 int32 = a__3 + b__4
    return t25, true
}

func maybe_total(flag__2 bool) Option__int32 {
    var native_value int32
    var native_ok bool
    native_value, native_ok = maybe_total__native(flag__2)
    if native_ok {
        return Some{
            _0: native_value,
        }
    }
    return None{}
}

func show(opt__5 Option__int32) string {
    var retv28 string
    var jp30 string
    switch opt__5.(type) {
    case None:
        jp30 = "none"
    case Some:
        var x4 int32 = opt__5.(Some)._0
        var value__6 int32 = x4
        var t31 string = int32_to_string(value__6)
        var t32 string = "some=" + t31
        jp30 = t32
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t34 Option__int32 = maybe_total(true)
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Option__int32 = maybe_total(false)
    var t37 string = show(t36)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
