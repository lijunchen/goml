package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop97:
    for {
        var t108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t109 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t108, 0)
        var jp99 bool
        if t109 {
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 1)
            jp99 = true
        } else {
            var t112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t113 bool = t112 < 4
            var jp111 bool
            if t113 {
                jp111 = true
            } else {
                jp111 = false
            }
            jp99 = jp111
        }
        if jp99 {
            var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
            var t101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t102 int = t100 + t101
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__1, t102)
            var t106 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t107 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t106, 1)
            if t107 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, 2)
                continue
            } else {
                var t104 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                var t105 int = t104 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t105)
                continue
            }
        } else {
            break Loop_loop97
        }
    }
    var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t85)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop88:
    for {
        var mtmp75 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
        var jp90 bool
        switch mtmp75 {
        case 0:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 1)
            jp90 = true
        case 1:
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__2, 2)
            jp90 = true
        case 2:
            jp90 = true
        default:
            jp90 = false
        }
        if jp90 {
            var t91 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
            var t92 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t93 int = t91 + t92
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total2__3, t93)
            var t95 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__2)
            var t96 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t95, 2)
            if t96 {
                break Loop_loop88
            } else {
                continue
            }
        } else {
            break Loop_loop88
        }
    }
    var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total2__3)
    println__T_int(t87)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv115 *ref_int_x
    var t116 *ref_int_x = ref__Ref_3int(value__207)
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv118 int
    var t119 int = ref_get__Ref_3int(self__208)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv121 bool
    var t122 bool = self__59 == other__60
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int_to_string(self__40)
    retv129 = t130
    return retv129
}

func main() {
    main0()
}
