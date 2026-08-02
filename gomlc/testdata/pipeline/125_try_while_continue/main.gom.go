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
    var t168 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    if t168 {
        return None{}
    } else {
        var t169 int32 = i__0 + 10
        var t170 Option__int32 = Some{
            _0: t169,
        }
        return t170
    }
}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop176:
    for {
        var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t178 bool = t177 < limit__1
        if t178 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t179 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t179)
            var t185 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t185 {
                continue
            } else {
                var mtmp157 Option__int32 = step(cur__4)
                var jp182 int32
                switch mtmp157.(type) {
                case None:
                    return None{}
                case Some:
                    var x158 int32 = mtmp157.(Some)._0
                    jp182 = x158
                    var t183 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t184 int32 = t183 + jp182
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t184)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop176
        }
    }
    var t174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t175 Option__int32 = Some{
        _0: t174,
    }
    return t175
}

func show(opt__6 Option__int32) string {
    switch opt__6.(type) {
    case None:
        return "none"
    case Some:
        var x161 int32 = opt__6.(Some)._0
        var t190 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x161)
        var t191 string = "some=" + t190
        return t191
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t193 Option__int32 = accumulate(2)
    var t194 string = show(t193)
    println__T_string(t194)
    var t195 Option__int32 = accumulate(4)
    var t196 string = show(t195)
    println__T_string(t196)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t199 bool = self__65 == other__66
    return t199
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t202 *ref_int32_x = ref__Ref_5int32(value__207)
    return t202
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t205 int32 = ref_get__Ref_5int32(self__208)
    return t205
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__6)
    return t210
}

func println__T_string(value__1 string) struct{} {
    var t212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
