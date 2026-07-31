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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func step(i__0 int32) Option__int32 {
    var retv162 Option__int32
    var t165 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    var jp164 Option__int32
    if t165 {
        jp164 = None{}
    } else {
        var t166 int32 = i__0 + 10
        var t167 Option__int32 = Some{
            _0: t166,
        }
        jp164 = t167
    }
    retv162 = jp164
    return retv162
}

func accumulate(limit__1 int32) Option__int32 {
    var retv169 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop173:
    for {
        var t174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t175 bool = t174 < limit__1
        if t175 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t176 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t176)
            var t182 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t182 {
                continue
            } else {
                var mtmp154 Option__int32 = step(cur__4)
                var jp179 int32
                switch mtmp154.(type) {
                case None:
                    retv169 = None{}
                    return retv169
                case Some:
                    var x155 int32 = mtmp154.(Some)._0
                    var try_value__43 int32 = x155
                    jp179 = try_value__43
                    var value__5 int32 = jp179
                    var t180 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t181 int32 = t180 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t181)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop173
        }
    }
    var t171 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t172 Option__int32 = Some{
        _0: t171,
    }
    retv169 = t172
    return retv169
}

func show(opt__6 Option__int32) string {
    var retv184 string
    var jp186 string
    switch opt__6.(type) {
    case None:
        jp186 = "none"
    case Some:
        var x158 int32 = opt__6.(Some)._0
        var value__7 int32 = x158
        var t187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t188 string = "some=" + t187
        jp186 = t188
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var t190 Option__int32 = accumulate(2)
    var t191 string = show(t190)
    println__T_string(t191)
    var t192 Option__int32 = accumulate(4)
    var t193 string = show(t192)
    println__T_string(t193)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv195 bool
    var t196 bool = self__65 == other__66
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv198 *ref_int32_x
    var t199 *ref_int32_x = ref__Ref_5int32(value__207)
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv201 int32
    var t202 int32 = ref_get__Ref_5int32(self__208)
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv206 string
    var t207 string = _goml_runtime_core_int32_to_string(self__6)
    retv206 = t207
    return retv206
}

func println__T_string(value__1 string) struct{} {
    var t209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv212 string
    retv212 = self__38
    return retv212
}

func main() {
    main0()
}
