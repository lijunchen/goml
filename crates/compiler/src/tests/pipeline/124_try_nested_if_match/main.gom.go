package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
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
    var retv13 Option__int32
    var jp15 Option__int32
    if flag__0 {
        var t16 Option__int32 = Some{
            _0: 8,
        }
        jp15 = t16
    } else {
        jp15 = None{}
    }
    retv13 = jp15
    return retv13
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv18 Option__int32
    var jp20 int32
    if top__1 {
        var jp23 int32
        switch mode__2 {
        case Take:
            var mtmp4 Option__int32 = maybe_num(inner_flag__3)
            var jp25 int32
            switch mtmp4.(type) {
            case None:
                retv18 = None{}
                return retv18
            case Some:
                var x5 int32 = mtmp4.(Some)._0
                var try_value__13 int32 = x5
                jp25 = try_value__13
                var inner__4 int32 = jp25
                var t26 int32 = inner__4 + 1
                jp23 = t26
                jp20 = jp23
                var value__6 int32 = jp20
                var t21 Option__int32 = Some{
                    _0: value__6,
                }
                retv18 = t21
                return retv18
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp23 = 20
            jp20 = jp23
            var value__6 int32 = jp20
            var t21 Option__int32 = Some{
                _0: value__6,
            }
            retv18 = t21
            return retv18
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp6 Option__int32 = maybe_num(inner_flag__3)
        var jp28 int32
        switch mtmp6.(type) {
        case None:
            retv18 = None{}
            return retv18
        case Some:
            var x7 int32 = mtmp6.(Some)._0
            var try_value__24 int32 = x7
            jp28 = try_value__24
            var inner__5 int32 = jp28
            var t29 int32 = inner__5 + 2
            jp20 = t29
            var value__6 int32 = jp20
            var t21 Option__int32 = Some{
                _0: value__6,
            }
            retv18 = t21
            return retv18
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv31 string
    var jp33 string
    switch opt__7.(type) {
    case None:
        jp33 = "none"
    case Some:
        var x8 int32 = opt__7.(Some)._0
        var value__8 int32 = x8
        var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t35 string = "some=" + t34
        jp33 = t35
    default:
        panic("non-exhaustive match")
    }
    retv31 = jp33
    return retv31
}

func main0() struct{} {
    var t37 Option__int32 = nested(true, Take, true)
    var t38 string = show(t37)
    println__T_string(t38)
    var t39 Option__int32 = nested(true, Skip, false)
    var t40 string = show(t39)
    println__T_string(t40)
    var t41 Option__int32 = nested(false, Take, false)
    var t42 string = show(t41)
    println__T_string(t42)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv44 string
    var t45 string = _goml_runtime_core_int32_to_string(self__2)
    retv44 = t45
    return retv44
}

func println__T_string(value__1 string) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
