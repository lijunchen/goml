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
    var retv73 Option__int32
    var jp75 Option__int32
    if flag__0 {
        var t76 Option__int32 = Some{
            _0: 8,
        }
        jp75 = t76
    } else {
        jp75 = None{}
    }
    retv73 = jp75
    return retv73
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv78 Option__int32
    var jp80 int32
    if top__1 {
        var jp83 int32
        switch mode__2 {
        case Take:
            var mtmp64 Option__int32 = maybe_num(inner_flag__3)
            var jp85 int32
            switch mtmp64.(type) {
            case None:
                retv78 = None{}
                return retv78
            case Some:
                var x65 int32 = mtmp64.(Some)._0
                var try_value__13 int32 = x65
                jp85 = try_value__13
                var inner__4 int32 = jp85
                var t86 int32 = inner__4 + 1
                jp83 = t86
                jp80 = jp83
                var value__6 int32 = jp80
                var t81 Option__int32 = Some{
                    _0: value__6,
                }
                retv78 = t81
                return retv78
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp83 = 20
            jp80 = jp83
            var value__6 int32 = jp80
            var t81 Option__int32 = Some{
                _0: value__6,
            }
            retv78 = t81
            return retv78
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp66 Option__int32 = maybe_num(inner_flag__3)
        var jp88 int32
        switch mtmp66.(type) {
        case None:
            retv78 = None{}
            return retv78
        case Some:
            var x67 int32 = mtmp66.(Some)._0
            var try_value__24 int32 = x67
            jp88 = try_value__24
            var inner__5 int32 = jp88
            var t89 int32 = inner__5 + 2
            jp80 = t89
            var value__6 int32 = jp80
            var t81 Option__int32 = Some{
                _0: value__6,
            }
            retv78 = t81
            return retv78
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv91 string
    var jp93 string
    switch opt__7.(type) {
    case None:
        jp93 = "none"
    case Some:
        var x68 int32 = opt__7.(Some)._0
        var value__8 int32 = x68
        var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t95 string = "some=" + t94
        jp93 = t95
    default:
        panic("non-exhaustive match")
    }
    retv91 = jp93
    return retv91
}

func main0() struct{} {
    var t97 Option__int32 = nested(true, Take, true)
    var t98 string = show(t97)
    println__T_string(t98)
    var t99 Option__int32 = nested(true, Skip, false)
    var t100 string = show(t99)
    println__T_string(t100)
    var t101 Option__int32 = nested(false, Take, false)
    var t102 string = show(t101)
    println__T_string(t102)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int32_to_string(self__6)
    retv104 = t105
    return retv104
}

func println__T_string(value__1 string) struct{} {
    var t107 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t107)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv110 string
    retv110 = self__38
    return retv110
}

func main() {
    main0()
}
