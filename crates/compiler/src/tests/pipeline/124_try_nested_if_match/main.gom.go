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
    var retv67 Option__int32
    var jp69 Option__int32
    if flag__0 {
        var t70 Option__int32 = Some{
            _0: 8,
        }
        jp69 = t70
    } else {
        jp69 = None{}
    }
    retv67 = jp69
    return retv67
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv72 Option__int32
    var jp74 int32
    if top__1 {
        var jp77 int32
        switch mode__2 {
        case Take:
            var mtmp58 Option__int32 = maybe_num(inner_flag__3)
            var jp79 int32
            switch mtmp58.(type) {
            case None:
                retv72 = None{}
                return retv72
            case Some:
                var x59 int32 = mtmp58.(Some)._0
                var try_value__13 int32 = x59
                jp79 = try_value__13
                var inner__4 int32 = jp79
                var t80 int32 = inner__4 + 1
                jp77 = t80
                jp74 = jp77
                var value__6 int32 = jp74
                var t75 Option__int32 = Some{
                    _0: value__6,
                }
                retv72 = t75
                return retv72
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp77 = 20
            jp74 = jp77
            var value__6 int32 = jp74
            var t75 Option__int32 = Some{
                _0: value__6,
            }
            retv72 = t75
            return retv72
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp60 Option__int32 = maybe_num(inner_flag__3)
        var jp82 int32
        switch mtmp60.(type) {
        case None:
            retv72 = None{}
            return retv72
        case Some:
            var x61 int32 = mtmp60.(Some)._0
            var try_value__24 int32 = x61
            jp82 = try_value__24
            var inner__5 int32 = jp82
            var t83 int32 = inner__5 + 2
            jp74 = t83
            var value__6 int32 = jp74
            var t75 Option__int32 = Some{
                _0: value__6,
            }
            retv72 = t75
            return retv72
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv85 string
    var jp87 string
    switch opt__7.(type) {
    case None:
        jp87 = "none"
    case Some:
        var x62 int32 = opt__7.(Some)._0
        var value__8 int32 = x62
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t89 string = "some=" + t88
        jp87 = t89
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main0() struct{} {
    var t91 Option__int32 = nested(true, Take, true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Option__int32 = nested(true, Skip, false)
    var t94 string = show(t93)
    println__T_string(t94)
    var t95 Option__int32 = nested(false, Take, false)
    var t96 string = show(t95)
    println__T_string(t96)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__2)
    retv98 = t99
    return retv98
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv104 string
    retv104 = self__34
    return retv104
}

func main() {
    main0()
}
