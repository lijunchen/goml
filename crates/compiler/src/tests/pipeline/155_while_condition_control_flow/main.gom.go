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
    Loop_loop107:
    for {
        var t113 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t114 bool = t113 < 3
        var jp109 bool
        if t114 {
            jp109 = true
        } else {
            jp109 = false
        }
        if jp109 {
            var t110 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            println__T_int32(t110)
            var t111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t112 int32 = t111 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t112)
            continue
        } else {
            break Loop_loop107
        }
    }
    var j__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop91:
    for {
        var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
        var t100 bool = t99 < 4
        var jp93 bool
        if t100 {
            var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t104 bool = t103 == 1
            var jp102 bool
            if t104 {
                jp102 = true
            } else {
                var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
                var t106 bool = t105 != 3
                jp102 = t106
            }
            jp93 = jp102
        } else {
            jp93 = false
        }
        if jp93 {
            var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
            var t95 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t96 int32 = t94 + t95
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t96)
            var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t98 int32 = t97 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__1, t98)
            continue
        } else {
            break Loop_loop91
        }
    }
    var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    println__T_int32(t76)
    var k__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var sum__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop79:
    for {
        var mtmp68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
        var jp81 bool
        switch mtmp68 {
        case 0:
            jp81 = true
        case 1:
            var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t90 bool = t89 == 0
            var jp88 bool
            if t90 {
                jp88 = true
            } else {
                jp88 = false
            }
            jp81 = jp88
        case 2:
            jp81 = true
        default:
            jp81 = false
        }
        if jp81 {
            var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t84 int32 = t82 + t83
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__4, t84)
            var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t86 int32 = t85 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(k__3, t86)
            continue
        } else {
            break Loop_loop79
        }
    }
    var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
    println__T_int32(t78)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv116 *ref_int32_x
    var t117 *ref_int32_x = ref__Ref_5int32(value__204)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv119 int32
    var t120 int32 = ref_get__Ref_5int32(self__205)
    retv119 = t120
    return retv119
}

func println__T_int32(value__1 int32) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv127 string
    var t128 string = _goml_runtime_core_int32_to_string(self__41)
    retv127 = t128
    return retv127
}

func main() {
    main0()
}
