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
    var retv69 Option__int32
    var jp71 Option__int32
    if flag__0 {
        var t72 Option__int32 = Some{
            _0: 3,
        }
        jp71 = t72
    } else {
        jp71 = None{}
    }
    retv69 = jp71
    return retv69
}

func maybe_double(value__1 int32) Option__int32 {
    var retv74 Option__int32
    var t77 bool = value__1 > 0
    var jp76 Option__int32
    if t77 {
        var t78 int32 = value__1 * 2
        var t79 Option__int32 = Some{
            _0: t78,
        }
        jp76 = t79
    } else {
        jp76 = None{}
    }
    retv74 = jp76
    return retv74
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv81 Option__int32
    var mtmp61 Option__int32 = maybe_seed(flag__2)
    var jp83 int32
    switch mtmp61.(type) {
    case None:
        retv81 = None{}
        return retv81
    case Some:
        var x62 int32 = mtmp61.(Some)._0
        var try_value__22 int32 = x62
        jp83 = try_value__22
        var a__3 int32 = jp83
        var mtmp63 Option__int32 = maybe_double(a__3)
        var jp85 int32
        switch mtmp63.(type) {
        case None:
            retv81 = None{}
            return retv81
        case Some:
            var x64 int32 = mtmp63.(Some)._0
            var try_value__26 int32 = x64
            jp85 = try_value__26
            var b__4 int32 = jp85
            var t86 int32 = a__3 + b__4
            var t87 Option__int32 = Some{
                _0: t86,
            }
            retv81 = t87
            return retv81
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv89 string
    var jp91 string
    switch opt__5.(type) {
    case None:
        jp91 = "none"
    case Some:
        var x65 int32 = opt__5.(Some)._0
        var value__6 int32 = x65
        var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t93 string = "some=" + t92
        jp91 = t93
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func main0() struct{} {
    var t95 Option__int32 = maybe_total(true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Option__int32 = maybe_total(false)
    var t98 string = show(t97)
    println__T_string(t98)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_int32_to_string(self__5)
    retv100 = t101
    return retv100
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv106 string
    retv106 = self__37
    return retv106
}

func main() {
    main0()
}
