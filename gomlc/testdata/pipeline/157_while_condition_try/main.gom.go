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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func step_some(i__0 int32) Option__bool {
    var retv166 Option__bool
    var t169 bool = i__0 < 3
    var jp168 Option__bool
    if t169 {
        var t170 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp168 = t170
    } else {
        var t171 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp168 = t171
    }
    retv166 = jp168
    return retv166
}

func step_none(i__1 int32) Option__bool {
    var retv173 Option__bool
    var t176 bool = i__1 < 2
    var jp175 Option__bool
    if t176 {
        var t177 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp175 = t177
    } else {
        jp175 = Option__bool_None{}
    }
    retv173 = jp175
    return retv173
}

func run_some() Option__int32 {
    var retv179 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop183:
    for {
        var t184 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp152 Option__bool = step_some(t184)
        var jp186 bool
        switch mtmp152.(type) {
        case Option__bool_None:
            retv179 = Option__int32_None{}
            return retv179
        case Option__bool_Some:
            var x153 bool = mtmp152.(Option__bool_Some)._0
            var try_value__31 bool = x153
            jp186 = try_value__31
            if jp186 {
                var t187 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t188 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t189 int32 = t187 + t188
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t189)
                var t190 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t191 int32 = t190 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t191)
                continue
            } else {
                break Loop_loop183
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t181 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t182 Option__int32 = Option__int32_Some{
        _0: t181,
    }
    retv179 = t182
    return retv179
}

func run_none() Option__int32 {
    var retv193 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop197:
    for {
        var t198 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp157 Option__bool = step_none(t198)
        var jp200 bool
        switch mtmp157.(type) {
        case Option__bool_None:
            retv193 = Option__int32_None{}
            return retv193
        case Option__bool_Some:
            var x158 bool = mtmp157.(Option__bool_Some)._0
            var try_value__67 bool = x158
            jp200 = try_value__67
            if jp200 {
                var t201 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t202 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t203 int32 = t201 + t202
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t203)
                var t204 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t205 int32 = t204 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t205)
                continue
            } else {
                break Loop_loop197
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t195 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t196 Option__int32 = Option__int32_Some{
        _0: t195,
    }
    retv193 = t196
    return retv193
}

func show(x__6 Option__int32) string {
    var retv207 string
    var jp209 string
    switch x__6.(type) {
    case Option__int32_None:
        jp209 = "none"
    case Option__int32_Some:
        var x162 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x162
        var t210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t211 string = "some=" + t210
        jp209 = t211
    default:
        panic("non-exhaustive match")
    }
    retv207 = jp209
    return retv207
}

func main0() struct{} {
    var t213 Option__int32 = run_some()
    var t214 string = show(t213)
    println__T_string(t214)
    var t215 Option__int32 = run_none()
    var t216 string = show(t215)
    println__T_string(t216)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv218 *ref_int32_x
    var t219 *ref_int32_x = ref__Ref_5int32(value__207)
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv221 int32
    var t222 int32 = ref_get__Ref_5int32(self__208)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv226 string
    var t227 string = _goml_runtime_core_int32_to_string(self__6)
    retv226 = t227
    return retv226
}

func println__T_string(value__1 string) struct{} {
    var t229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t229)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv232 string
    retv232 = self__38
    return retv232
}

func main() {
    main0()
}
