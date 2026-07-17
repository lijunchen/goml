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
    Loop_loop87:
    for {
        var t98 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t99 bool = t98 == 0
        var jp89 bool
        if t99 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 1)
            jp89 = true
        } else {
            var t102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t103 bool = t102 < 4
            var jp101 bool
            if t103 {
                jp101 = true
            } else {
                jp101 = false
            }
            jp89 = jp101
        }
        if jp89 {
            var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
            var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t92 int32 = t90 + t91
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__1, t92)
            var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t97 bool = t96 == 1
            if t97 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, 2)
                continue
            } else {
                var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                var t95 int32 = t94 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t95)
                continue
            }
        } else {
            break Loop_loop87
        }
    }
    var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__1)
    println__T_int32(t75)
    var j__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total2__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop78:
    for {
        var mtmp65 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
        var jp80 bool
        switch mtmp65 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 1)
            jp80 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__2, 2)
            jp80 = true
        case 2:
            jp80 = true
        default:
            jp80 = false
        }
        if jp80 {
            var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
            var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t83 int32 = t81 + t82
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total2__3, t83)
            var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__2)
            var t86 bool = t85 == 2
            if t86 {
                break Loop_loop78
            } else {
                continue
            }
        } else {
            break Loop_loop78
        }
    }
    var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total2__3)
    println__T_int32(t77)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv105 *ref_int32_x
    var t106 *ref_int32_x = ref__Ref_5int32(value__201)
    retv105 = t106
    return retv105
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv108 int32
    var t109 int32 = ref_get__Ref_5int32(self__202)
    retv108 = t109
    return retv108
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t113 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t113)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__38)
    retv116 = t117
    return retv116
}

func main() {
    main0()
}
