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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_num(flag__0 bool) Option__int32 {
    var retv9 Option__int32
    var jp11 Option__int32
    if flag__0 {
        var t12 Option__int32 = Some{
            _0: 8,
        }
        jp11 = t12
    } else {
        jp11 = None{}
    }
    retv9 = jp11
    return retv9
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv14 Option__int32
    var jp16 int32
    if top__1 {
        var jp19 int32
        switch mode__2 {
        case Take:
            var mtmp0 Option__int32 = maybe_num(inner_flag__3)
            var jp21 int32
            switch mtmp0.(type) {
            case None:
                retv14 = None{}
                return retv14
            case Some:
                var x1 int32 = mtmp0.(Some)._0
                var try_value__13 int32 = x1
                jp21 = try_value__13
                var inner__4 int32 = jp21
                var t22 int32 = inner__4 + 1
                jp19 = t22
                jp16 = jp19
                var value__6 int32 = jp16
                var t17 Option__int32 = Some{
                    _0: value__6,
                }
                retv14 = t17
                return retv14
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp19 = 20
            jp16 = jp19
            var value__6 int32 = jp16
            var t17 Option__int32 = Some{
                _0: value__6,
            }
            retv14 = t17
            return retv14
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp2 Option__int32 = maybe_num(inner_flag__3)
        var jp24 int32
        switch mtmp2.(type) {
        case None:
            retv14 = None{}
            return retv14
        case Some:
            var x3 int32 = mtmp2.(Some)._0
            var try_value__24 int32 = x3
            jp24 = try_value__24
            var inner__5 int32 = jp24
            var t25 int32 = inner__5 + 2
            jp16 = t25
            var value__6 int32 = jp16
            var t17 Option__int32 = Some{
                _0: value__6,
            }
            retv14 = t17
            return retv14
        default:
            panic("non-exhaustive match")
        }
    }
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
    var t33 Option__int32 = nested(true, Take, true)
    var t34 string = show(t33)
    println__T_string(t34)
    var t35 Option__int32 = nested(true, Skip, false)
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Option__int32 = nested(false, Take, false)
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
