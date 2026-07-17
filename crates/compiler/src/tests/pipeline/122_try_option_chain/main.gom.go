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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_seed(flag__0 bool) Option__int32 {
    var retv66 Option__int32
    var jp68 Option__int32
    if flag__0 {
        var t69 Option__int32 = Some{
            _0: 3,
        }
        jp68 = t69
    } else {
        jp68 = None{}
    }
    retv66 = jp68
    return retv66
}

func maybe_double(value__1 int32) Option__int32 {
    var retv71 Option__int32
    var t74 bool = value__1 > 0
    var jp73 Option__int32
    if t74 {
        var t75 int32 = value__1 * 2
        var t76 Option__int32 = Some{
            _0: t75,
        }
        jp73 = t76
    } else {
        jp73 = None{}
    }
    retv71 = jp73
    return retv71
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv78 Option__int32
    var mtmp58 Option__int32 = maybe_seed(flag__2)
    var jp80 int32
    switch mtmp58.(type) {
    case None:
        retv78 = None{}
        return retv78
    case Some:
        var x59 int32 = mtmp58.(Some)._0
        var try_value__22 int32 = x59
        jp80 = try_value__22
        var a__3 int32 = jp80
        var mtmp60 Option__int32 = maybe_double(a__3)
        var jp82 int32
        switch mtmp60.(type) {
        case None:
            retv78 = None{}
            return retv78
        case Some:
            var x61 int32 = mtmp60.(Some)._0
            var try_value__26 int32 = x61
            jp82 = try_value__26
            var b__4 int32 = jp82
            var t83 int32 = a__3 + b__4
            var t84 Option__int32 = Some{
                _0: t83,
            }
            retv78 = t84
            return retv78
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv86 string
    var jp88 string
    switch opt__5.(type) {
    case None:
        jp88 = "none"
    case Some:
        var x62 int32 = opt__5.(Some)._0
        var value__6 int32 = x62
        var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t90 string = "some=" + t89
        jp88 = t90
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t92 Option__int32 = maybe_total(true)
    var t93 string = show(t92)
    println__T_string(t93)
    var t94 Option__int32 = maybe_total(false)
    var t95 string = show(t94)
    println__T_string(t95)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__2)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv103 string
    retv103 = self__34
    return retv103
}

func main() {
    main0()
}
