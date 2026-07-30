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
    var retv76 Option__int32
    var jp78 Option__int32
    if flag__0 {
        var t79 Option__int32 = Some{
            _0: 3,
        }
        jp78 = t79
    } else {
        jp78 = None{}
    }
    retv76 = jp78
    return retv76
}

func maybe_double(value__1 int32) Option__int32 {
    var retv81 Option__int32
    var t84 bool = value__1 > 0
    var jp83 Option__int32
    if t84 {
        var t85 int32 = value__1 * 2
        var t86 Option__int32 = Some{
            _0: t85,
        }
        jp83 = t86
    } else {
        jp83 = None{}
    }
    retv81 = jp83
    return retv81
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv88 Option__int32
    var mtmp68 Option__int32 = maybe_seed(flag__2)
    var jp90 int32
    switch mtmp68.(type) {
    case None:
        retv88 = None{}
        return retv88
    case Some:
        var x69 int32 = mtmp68.(Some)._0
        var try_value__22 int32 = x69
        jp90 = try_value__22
        var a__3 int32 = jp90
        var mtmp70 Option__int32 = maybe_double(a__3)
        var jp92 int32
        switch mtmp70.(type) {
        case None:
            retv88 = None{}
            return retv88
        case Some:
            var x71 int32 = mtmp70.(Some)._0
            var try_value__26 int32 = x71
            jp92 = try_value__26
            var b__4 int32 = jp92
            var t93 int32 = a__3 + b__4
            var t94 Option__int32 = Some{
                _0: t93,
            }
            retv88 = t94
            return retv88
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv96 string
    var jp98 string
    switch opt__5.(type) {
    case None:
        jp98 = "none"
    case Some:
        var x72 int32 = opt__5.(Some)._0
        var value__6 int32 = x72
        var t99 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t100 string = "some=" + t99
        jp98 = t100
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func main0() struct{} {
    var t102 Option__int32 = maybe_total(true)
    var t103 string = show(t102)
    println__T_string(t103)
    var t104 Option__int32 = maybe_total(false)
    var t105 string = show(t104)
    println__T_string(t105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__6)
    retv107 = t108
    return retv107
}

func println__T_string(value__1 string) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv113 string
    retv113 = self__38
    return retv113
}

func main() {
    main0()
}
