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

func main0() struct{} {
    var i__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop69:
    for {
        var t70 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t71 bool = t70 < 8
        if t71 {
            var t72 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t73 int32 = t72 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t73)
            var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t80 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t79, 3)
            if t80 {
                continue
            } else {
                var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t78 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t77, 6)
                if t78 {
                    continue
                } else {
                    var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                    println__T_int32(t76)
                    continue
                }
            }
        } else {
            break Loop_loop69
        }
    }
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv82 *ref_int32_x
    var t83 *ref_int32_x = ref__Ref_5int32(value__204)
    retv82 = t83
    return retv82
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv85 int32
    var t86 int32 = ref_get__Ref_5int32(self__205)
    retv85 = t86
    return retv85
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv90 bool
    var t91 bool = self__61 == other__62
    retv90 = t91
    return retv90
}

func println__T_int32(value__1 int32) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__41)
    retv99 = t100
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv102 string
    retv102 = self__37
    return retv102
}

func main() {
    main0()
}
