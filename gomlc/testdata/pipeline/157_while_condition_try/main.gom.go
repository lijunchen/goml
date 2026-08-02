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
    var t172 bool = i__0 < 3
    if t172 {
        var t173 Option__bool = Option__bool_Some{
            _0: true,
        }
        return t173
    } else {
        var t174 Option__bool = Option__bool_Some{
            _0: false,
        }
        return t174
    }
}

func step_none(i__1 int32) Option__bool {
    var t179 bool = i__1 < 2
    if t179 {
        var t180 Option__bool = Option__bool_Some{
            _0: true,
        }
        return t180
    } else {
        return Option__bool_None{}
    }
}

func run_some() Option__int32 {
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop186:
    for {
        var t187 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp155 Option__bool = step_some(t187)
        var jp189 bool
        switch mtmp155.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x156 bool = mtmp155.(Option__bool_Some)._0
            jp189 = x156
            if jp189 {
                var t190 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t191 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t192 int32 = t190 + t191
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t192)
                var t193 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t194 int32 = t193 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t194)
                continue
            } else {
                break Loop_loop186
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t184 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t185 Option__int32 = Option__int32_Some{
        _0: t184,
    }
    return t185
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop200:
    for {
        var t201 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp160 Option__bool = step_none(t201)
        var jp203 bool
        switch mtmp160.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x161 bool = mtmp160.(Option__bool_Some)._0
            jp203 = x161
            if jp203 {
                var t204 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t205 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t206 int32 = t204 + t205
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t206)
                var t207 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t208 int32 = t207 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t208)
                continue
            } else {
                break Loop_loop200
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t198 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t199 Option__int32 = Option__int32_Some{
        _0: t198,
    }
    return t199
}

func show(x__6 Option__int32) string {
    switch x__6.(type) {
    case Option__int32_None:
        return "none"
    case Option__int32_Some:
        var x165 int32 = x__6.(Option__int32_Some)._0
        var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x165)
        var t214 string = "some=" + t213
        return t214
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t216 Option__int32 = run_some()
    var t217 string = show(t216)
    println__T_string(t217)
    var t218 Option__int32 = run_none()
    var t219 string = show(t218)
    println__T_string(t219)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t222 *ref_int32_x = ref__Ref_5int32(value__207)
    return t222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t225 int32 = ref_get__Ref_5int32(self__208)
    return t225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t230 string = _goml_runtime_core_int32_to_string(self__6)
    return t230
}

func println__T_string(value__1 string) struct{} {
    var t232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t232)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
