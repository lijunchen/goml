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
    var retv70 Option__int32
    var jp72 Option__int32
    if flag__0 {
        var t73 Option__int32 = Some{
            _0: 8,
        }
        jp72 = t73
    } else {
        jp72 = None{}
    }
    retv70 = jp72
    return retv70
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv75 Option__int32
    var jp77 int32
    if top__1 {
        var jp80 int32
        switch mode__2 {
        case Take:
            var mtmp61 Option__int32 = maybe_num(inner_flag__3)
            var jp82 int32
            switch mtmp61.(type) {
            case None:
                retv75 = None{}
                return retv75
            case Some:
                var x62 int32 = mtmp61.(Some)._0
                var try_value__13 int32 = x62
                jp82 = try_value__13
                var inner__4 int32 = jp82
                var t83 int32 = inner__4 + 1
                jp80 = t83
                jp77 = jp80
                var value__6 int32 = jp77
                var t78 Option__int32 = Some{
                    _0: value__6,
                }
                retv75 = t78
                return retv75
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp80 = 20
            jp77 = jp80
            var value__6 int32 = jp77
            var t78 Option__int32 = Some{
                _0: value__6,
            }
            retv75 = t78
            return retv75
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp63 Option__int32 = maybe_num(inner_flag__3)
        var jp85 int32
        switch mtmp63.(type) {
        case None:
            retv75 = None{}
            return retv75
        case Some:
            var x64 int32 = mtmp63.(Some)._0
            var try_value__24 int32 = x64
            jp85 = try_value__24
            var inner__5 int32 = jp85
            var t86 int32 = inner__5 + 2
            jp77 = t86
            var value__6 int32 = jp77
            var t78 Option__int32 = Some{
                _0: value__6,
            }
            retv75 = t78
            return retv75
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv88 string
    var jp90 string
    switch opt__7.(type) {
    case None:
        jp90 = "none"
    case Some:
        var x65 int32 = opt__7.(Some)._0
        var value__8 int32 = x65
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t92 string = "some=" + t91
        jp90 = t92
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func main0() struct{} {
    var t94 Option__int32 = nested(true, Take, true)
    var t95 string = show(t94)
    println__T_string(t95)
    var t96 Option__int32 = nested(true, Skip, false)
    var t97 string = show(t96)
    println__T_string(t97)
    var t98 Option__int32 = nested(false, Take, false)
    var t99 string = show(t98)
    println__T_string(t99)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__5)
    retv101 = t102
    return retv101
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv107 string
    retv107 = self__37
    return retv107
}

func main() {
    main0()
}
