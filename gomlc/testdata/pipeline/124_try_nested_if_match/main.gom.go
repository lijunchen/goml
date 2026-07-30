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
    var retv117 Option__int32
    var jp119 Option__int32
    if flag__0 {
        var t120 Option__int32 = Some{
            _0: 8,
        }
        jp119 = t120
    } else {
        jp119 = None{}
    }
    retv117 = jp119
    return retv117
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv122 Option__int32
    var jp124 int32
    if top__1 {
        var jp127 int32
        switch mode__2 {
        case Take:
            var mtmp108 Option__int32 = maybe_num(inner_flag__3)
            var jp129 int32
            switch mtmp108.(type) {
            case None:
                retv122 = None{}
                return retv122
            case Some:
                var x109 int32 = mtmp108.(Some)._0
                var try_value__13 int32 = x109
                jp129 = try_value__13
                var inner__4 int32 = jp129
                var t130 int32 = inner__4 + 1
                jp127 = t130
                jp124 = jp127
                var value__6 int32 = jp124
                var t125 Option__int32 = Some{
                    _0: value__6,
                }
                retv122 = t125
                return retv122
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp127 = 20
            jp124 = jp127
            var value__6 int32 = jp124
            var t125 Option__int32 = Some{
                _0: value__6,
            }
            retv122 = t125
            return retv122
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp110 Option__int32 = maybe_num(inner_flag__3)
        var jp132 int32
        switch mtmp110.(type) {
        case None:
            retv122 = None{}
            return retv122
        case Some:
            var x111 int32 = mtmp110.(Some)._0
            var try_value__24 int32 = x111
            jp132 = try_value__24
            var inner__5 int32 = jp132
            var t133 int32 = inner__5 + 2
            jp124 = t133
            var value__6 int32 = jp124
            var t125 Option__int32 = Some{
                _0: value__6,
            }
            retv122 = t125
            return retv122
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv135 string
    var jp137 string
    switch opt__7.(type) {
    case None:
        jp137 = "none"
    case Some:
        var x112 int32 = opt__7.(Some)._0
        var value__8 int32 = x112
        var t138 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t139 string = "some=" + t138
        jp137 = t139
    default:
        panic("non-exhaustive match")
    }
    retv135 = jp137
    return retv135
}

func main0() struct{} {
    var t141 Option__int32 = nested(true, Take, true)
    var t142 string = show(t141)
    println__T_string(t142)
    var t143 Option__int32 = nested(true, Skip, false)
    var t144 string = show(t143)
    println__T_string(t144)
    var t145 Option__int32 = nested(false, Take, false)
    var t146 string = show(t145)
    println__T_string(t146)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv148 string
    var t149 string = _goml_runtime_core_int32_to_string(self__6)
    retv148 = t149
    return retv148
}

func println__T_string(value__1 string) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t151)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv154 string
    retv154 = self__38
    return retv154
}

func main() {
    main0()
}
