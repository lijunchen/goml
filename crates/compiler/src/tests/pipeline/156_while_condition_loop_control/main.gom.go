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
    var total__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop90:
    for {
        var t101 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t102 bool = t101 == 0
        var jp92 bool
        if t102 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 1)
            jp92 = true
        } else {
            var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t106 bool = t105 < 4
            var jp104 bool
            if t106 {
                jp104 = true
            } else {
                jp104 = false
            }
            jp92 = jp104
        }
        if jp92 {
            var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
            var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t95 int32 = t93 + t94
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__1, t95)
            var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t100 bool = t99 == 1
            if t100 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 2)
                continue
            } else {
                var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t98 int32 = t97 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t98)
                continue
            }
        } else {
            break Loop_loop90
        }
    }
    var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
    println__T_int32(t78)
    var j__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total2__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop81:
    for {
        var mtmp68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
        var jp83 bool
        switch mtmp68 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 1)
            jp83 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 2)
            jp83 = true
        case 2:
            jp83 = true
        default:
            jp83 = false
        }
        if jp83 {
            var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
            var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t86 int32 = t84 + t85
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total2__3, t86)
            var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t89 bool = t88 == 2
            if t89 {
                break Loop_loop81
            } else {
                continue
            }
        } else {
            break Loop_loop81
        }
    }
    var t80 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
    println__T_int32(t80)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv108 *ref_int32_x
    var t109 *ref_int32_x = ref__Ref_5int32(value__204)
    retv108 = t109
    return retv108
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv111 int32
    var t112 int32 = ref_get__Ref_5int32(self__205)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int32_to_string(self__41)
    retv119 = t120
    return retv119
}

func main() {
    main0()
}
