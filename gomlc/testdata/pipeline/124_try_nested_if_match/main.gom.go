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
    var retv77 Option__int32
    var jp79 Option__int32
    if flag__0 {
        var t80 Option__int32 = Some{
            _0: 8,
        }
        jp79 = t80
    } else {
        jp79 = None{}
    }
    retv77 = jp79
    return retv77
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv82 Option__int32
    var jp84 int32
    if top__1 {
        var jp87 int32
        switch mode__2 {
        case Take:
            var mtmp68 Option__int32 = maybe_num(inner_flag__3)
            var jp89 int32
            switch mtmp68.(type) {
            case None:
                retv82 = None{}
                return retv82
            case Some:
                var x69 int32 = mtmp68.(Some)._0
                var try_value__13 int32 = x69
                jp89 = try_value__13
                var inner__4 int32 = jp89
                var t90 int32 = inner__4 + 1
                jp87 = t90
                jp84 = jp87
                var value__6 int32 = jp84
                var t85 Option__int32 = Some{
                    _0: value__6,
                }
                retv82 = t85
                return retv82
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp87 = 20
            jp84 = jp87
            var value__6 int32 = jp84
            var t85 Option__int32 = Some{
                _0: value__6,
            }
            retv82 = t85
            return retv82
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp70 Option__int32 = maybe_num(inner_flag__3)
        var jp92 int32
        switch mtmp70.(type) {
        case None:
            retv82 = None{}
            return retv82
        case Some:
            var x71 int32 = mtmp70.(Some)._0
            var try_value__24 int32 = x71
            jp92 = try_value__24
            var inner__5 int32 = jp92
            var t93 int32 = inner__5 + 2
            jp84 = t93
            var value__6 int32 = jp84
            var t85 Option__int32 = Some{
                _0: value__6,
            }
            retv82 = t85
            return retv82
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv95 string
    var jp97 string
    switch opt__7.(type) {
    case None:
        jp97 = "none"
    case Some:
        var x72 int32 = opt__7.(Some)._0
        var value__8 int32 = x72
        var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t99 string = "some=" + t98
        jp97 = t99
    default:
        panic("non-exhaustive match")
    }
    retv95 = jp97
    return retv95
}

func main0() struct{} {
    var t101 Option__int32 = nested(true, Take, true)
    var t102 string = show(t101)
    println__T_string(t102)
    var t103 Option__int32 = nested(true, Skip, false)
    var t104 string = show(t103)
    println__T_string(t104)
    var t105 Option__int32 = nested(false, Take, false)
    var t106 string = show(t105)
    println__T_string(t106)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv108 string
    var t109 string = _goml_runtime_core_int32_to_string(self__6)
    retv108 = t109
    return retv108
}

func println__T_string(value__1 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func main() {
    main0()
}
