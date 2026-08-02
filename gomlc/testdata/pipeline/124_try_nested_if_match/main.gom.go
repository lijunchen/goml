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
    var retv164 Option__int32
    var jp166 Option__int32
    if flag__0 {
        var t167 Option__int32 = Some{
            _0: 8,
        }
        jp166 = t167
    } else {
        jp166 = None{}
    }
    retv164 = jp166
    return retv164
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv169 Option__int32
    var jp171 int32
    if top__1 {
        var jp174 int32
        switch mode__2 {
        case Take:
            var mtmp155 Option__int32 = maybe_num(inner_flag__3)
            var jp176 int32
            switch mtmp155.(type) {
            case None:
                retv169 = None{}
                return retv169
            case Some:
                var x156 int32 = mtmp155.(Some)._0
                var try_value__13 int32 = x156
                jp176 = try_value__13
                var inner__4 int32 = jp176
                var t177 int32 = inner__4 + 1
                jp174 = t177
                jp171 = jp174
                var value__6 int32 = jp171
                var t172 Option__int32 = Some{
                    _0: value__6,
                }
                retv169 = t172
                return retv169
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp174 = 20
            jp171 = jp174
            var value__6 int32 = jp171
            var t172 Option__int32 = Some{
                _0: value__6,
            }
            retv169 = t172
            return retv169
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp157 Option__int32 = maybe_num(inner_flag__3)
        var jp179 int32
        switch mtmp157.(type) {
        case None:
            retv169 = None{}
            return retv169
        case Some:
            var x158 int32 = mtmp157.(Some)._0
            var try_value__24 int32 = x158
            jp179 = try_value__24
            var inner__5 int32 = jp179
            var t180 int32 = inner__5 + 2
            jp171 = t180
            var value__6 int32 = jp171
            var t172 Option__int32 = Some{
                _0: value__6,
            }
            retv169 = t172
            return retv169
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv182 string
    var jp184 string
    switch opt__7.(type) {
    case None:
        jp184 = "none"
    case Some:
        var x159 int32 = opt__7.(Some)._0
        var value__8 int32 = x159
        var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t186 string = "some=" + t185
        jp184 = t186
    default:
        panic("non-exhaustive match")
    }
    retv182 = jp184
    return retv182
}

func main0() struct{} {
    var t188 Option__int32 = nested(true, Take, true)
    var t189 string = show(t188)
    println__T_string(t189)
    var t190 Option__int32 = nested(true, Skip, false)
    var t191 string = show(t190)
    println__T_string(t191)
    var t192 Option__int32 = nested(false, Take, false)
    var t193 string = show(t192)
    println__T_string(t193)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    retv195 = t196
    return retv195
}

func println__T_string(value__1 string) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv201 string
    retv201 = self__38
    return retv201
}

func main() {
    main0()
}
