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
    var retv118 Option__int32
    var t121 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    var jp120 Option__int32
    if t121 {
        jp120 = None{}
    } else {
        var t122 int32 = i__0 + 10
        var t123 Option__int32 = Some{
            _0: t122,
        }
        jp120 = t123
    }
    retv118 = jp120
    return retv118
}

func accumulate(limit__1 int32) Option__int32 {
    var retv125 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop129:
    for {
        var t130 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t131 bool = t130 < limit__1
        if t131 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t132 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t132)
            var t138 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t138 {
                continue
            } else {
                var mtmp110 Option__int32 = step(cur__4)
                var jp135 int32
                switch mtmp110.(type) {
                case None:
                    retv125 = None{}
                    return retv125
                case Some:
                    var x111 int32 = mtmp110.(Some)._0
                    var try_value__43 int32 = x111
                    jp135 = try_value__43
                    var value__5 int32 = jp135
                    var t136 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t137 int32 = t136 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t137)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop129
        }
    }
    var t127 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t128 Option__int32 = Some{
        _0: t127,
    }
    retv125 = t128
    return retv125
}

func show(opt__6 Option__int32) string {
    var retv140 string
    var jp142 string
    switch opt__6.(type) {
    case None:
        jp142 = "none"
    case Some:
        var x114 int32 = opt__6.(Some)._0
        var value__7 int32 = x114
        var t143 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t144 string = "some=" + t143
        jp142 = t144
    default:
        panic("non-exhaustive match")
    }
    retv140 = jp142
    return retv140
}

func main0() struct{} {
    var t146 Option__int32 = accumulate(2)
    var t147 string = show(t146)
    println__T_string(t147)
    var t148 Option__int32 = accumulate(4)
    var t149 string = show(t148)
    println__T_string(t149)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv151 bool
    var t152 bool = self__65 == other__66
    retv151 = t152
    return retv151
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv154 *ref_int32_x
    var t155 *ref_int32_x = ref__Ref_5int32(value__207)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv157 int32
    var t158 int32 = ref_get__Ref_5int32(self__208)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv162 string
    var t163 string = _goml_runtime_core_int32_to_string(self__6)
    retv162 = t163
    return retv162
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv168 string
    retv168 = self__38
    return retv168
}

func main() {
    main0()
}
