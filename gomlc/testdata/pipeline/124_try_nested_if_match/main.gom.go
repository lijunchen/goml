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
    var retv161 Option__int32
    var jp163 Option__int32
    if flag__0 {
        var t164 Option__int32 = Some{
            _0: 8,
        }
        jp163 = t164
    } else {
        jp163 = None{}
    }
    retv161 = jp163
    return retv161
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv166 Option__int32
    var jp168 int32
    if top__1 {
        var jp171 int32
        switch mode__2 {
        case Take:
            var mtmp152 Option__int32 = maybe_num(inner_flag__3)
            var jp173 int32
            switch mtmp152.(type) {
            case None:
                retv166 = None{}
                return retv166
            case Some:
                var x153 int32 = mtmp152.(Some)._0
                var try_value__13 int32 = x153
                jp173 = try_value__13
                var inner__4 int32 = jp173
                var t174 int32 = inner__4 + 1
                jp171 = t174
                jp168 = jp171
                var value__6 int32 = jp168
                var t169 Option__int32 = Some{
                    _0: value__6,
                }
                retv166 = t169
                return retv166
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp171 = 20
            jp168 = jp171
            var value__6 int32 = jp168
            var t169 Option__int32 = Some{
                _0: value__6,
            }
            retv166 = t169
            return retv166
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp154 Option__int32 = maybe_num(inner_flag__3)
        var jp176 int32
        switch mtmp154.(type) {
        case None:
            retv166 = None{}
            return retv166
        case Some:
            var x155 int32 = mtmp154.(Some)._0
            var try_value__24 int32 = x155
            jp176 = try_value__24
            var inner__5 int32 = jp176
            var t177 int32 = inner__5 + 2
            jp168 = t177
            var value__6 int32 = jp168
            var t169 Option__int32 = Some{
                _0: value__6,
            }
            retv166 = t169
            return retv166
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv179 string
    var jp181 string
    switch opt__7.(type) {
    case None:
        jp181 = "none"
    case Some:
        var x156 int32 = opt__7.(Some)._0
        var value__8 int32 = x156
        var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t183 string = "some=" + t182
        jp181 = t183
    default:
        panic("non-exhaustive match")
    }
    retv179 = jp181
    return retv179
}

func main0() struct{} {
    var t185 Option__int32 = nested(true, Take, true)
    var t186 string = show(t185)
    println__T_string(t186)
    var t187 Option__int32 = nested(true, Skip, false)
    var t188 string = show(t187)
    println__T_string(t188)
    var t189 Option__int32 = nested(false, Take, false)
    var t190 string = show(t189)
    println__T_string(t190)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv192 string
    var t193 string = _goml_runtime_core_int32_to_string(self__6)
    retv192 = t193
    return retv192
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
