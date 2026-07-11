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
    var retv16 Option__int32
    var jp18 Option__int32
    if flag__0 {
        var t19 Option__int32 = Some{
            _0: 8,
        }
        jp18 = t19
    } else {
        jp18 = None{}
    }
    retv16 = jp18
    return retv16
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv21 Option__int32
    var jp23 int32
    if top__1 {
        var jp26 int32
        switch mode__2 {
        case Take:
            var mtmp7 Option__int32 = maybe_num(inner_flag__3)
            var jp28 int32
            switch mtmp7.(type) {
            case None:
                retv21 = None{}
                return retv21
            case Some:
                var x8 int32 = mtmp7.(Some)._0
                var try_value__13 int32 = x8
                jp28 = try_value__13
                var inner__4 int32 = jp28
                var t29 int32 = inner__4 + 1
                jp26 = t29
                jp23 = jp26
                var value__6 int32 = jp23
                var t24 Option__int32 = Some{
                    _0: value__6,
                }
                retv21 = t24
                return retv21
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp26 = 20
            jp23 = jp26
            var value__6 int32 = jp23
            var t24 Option__int32 = Some{
                _0: value__6,
            }
            retv21 = t24
            return retv21
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp9 Option__int32 = maybe_num(inner_flag__3)
        var jp31 int32
        switch mtmp9.(type) {
        case None:
            retv21 = None{}
            return retv21
        case Some:
            var x10 int32 = mtmp9.(Some)._0
            var try_value__24 int32 = x10
            jp31 = try_value__24
            var inner__5 int32 = jp31
            var t32 int32 = inner__5 + 2
            jp23 = t32
            var value__6 int32 = jp23
            var t24 Option__int32 = Some{
                _0: value__6,
            }
            retv21 = t24
            return retv21
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv34 string
    var jp36 string
    switch opt__7.(type) {
    case None:
        jp36 = "none"
    case Some:
        var x11 int32 = opt__7.(Some)._0
        var value__8 int32 = x11
        var t37 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t38 string = "some=" + t37
        jp36 = t38
    default:
        panic("non-exhaustive match")
    }
    retv34 = jp36
    return retv34
}

func main0() struct{} {
    var t40 Option__int32 = nested(true, Take, true)
    var t41 string = show(t40)
    println__T_string(t41)
    var t42 Option__int32 = nested(true, Skip, false)
    var t43 string = show(t42)
    println__T_string(t43)
    var t44 Option__int32 = nested(false, Take, false)
    var t45 string = show(t44)
    println__T_string(t45)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int32_to_string(self__2)
    retv47 = t48
    return retv47
}

func println__T_string(value__1 string) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv53 string
    retv53 = self__9
    return retv53
}

func main() {
    main0()
}
