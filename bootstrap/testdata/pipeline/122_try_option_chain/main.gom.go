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
    var retv72 Option__int32
    var jp74 Option__int32
    if flag__0 {
        var t75 Option__int32 = Some{
            _0: 3,
        }
        jp74 = t75
    } else {
        jp74 = None{}
    }
    retv72 = jp74
    return retv72
}

func maybe_double(value__1 int32) Option__int32 {
    var retv77 Option__int32
    var t80 bool = value__1 > 0
    var jp79 Option__int32
    if t80 {
        var t81 int32 = value__1 * 2
        var t82 Option__int32 = Some{
            _0: t81,
        }
        jp79 = t82
    } else {
        jp79 = None{}
    }
    retv77 = jp79
    return retv77
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv84 Option__int32
    var mtmp64 Option__int32 = maybe_seed(flag__2)
    var jp86 int32
    switch mtmp64.(type) {
    case None:
        retv84 = None{}
        return retv84
    case Some:
        var x65 int32 = mtmp64.(Some)._0
        var try_value__22 int32 = x65
        jp86 = try_value__22
        var a__3 int32 = jp86
        var mtmp66 Option__int32 = maybe_double(a__3)
        var jp88 int32
        switch mtmp66.(type) {
        case None:
            retv84 = None{}
            return retv84
        case Some:
            var x67 int32 = mtmp66.(Some)._0
            var try_value__26 int32 = x67
            jp88 = try_value__26
            var b__4 int32 = jp88
            var t89 int32 = a__3 + b__4
            var t90 Option__int32 = Some{
                _0: t89,
            }
            retv84 = t90
            return retv84
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv92 string
    var jp94 string
    switch opt__5.(type) {
    case None:
        jp94 = "none"
    case Some:
        var x68 int32 = opt__5.(Some)._0
        var value__6 int32 = x68
        var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t96 string = "some=" + t95
        jp94 = t96
    default:
        panic("non-exhaustive match")
    }
    retv92 = jp94
    return retv92
}

func main0() struct{} {
    var t98 Option__int32 = maybe_total(true)
    var t99 string = show(t98)
    println__T_string(t99)
    var t100 Option__int32 = maybe_total(false)
    var t101 string = show(t100)
    println__T_string(t101)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv103 string
    var t104 string = _goml_runtime_core_int32_to_string(self__6)
    retv103 = t104
    return retv103
}

func println__T_string(value__1 string) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv109 string
    retv109 = self__38
    return retv109
}

func main() {
    main0()
}
