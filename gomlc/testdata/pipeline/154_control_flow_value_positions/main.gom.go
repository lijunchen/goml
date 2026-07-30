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
    var sum__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop130:
    for {
        var t131 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t132 bool = t131 < 5
        if t132 {
            var t133 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t134 int = t133 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t134)
            var t139 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t140 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t139, 3)
            var jp136 int
            if t140 {
                continue
            } else {
                var t141 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
                jp136 = t141
                var cur__2 int = jp136
                var t137 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
                var t138 int = t137 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__1, t138)
                continue
            }
        } else {
            break Loop_loop130
        }
    }
    var t119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__1)
    println__T_int(t119)
    var j__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop122:
    for {
        if true {
            var t123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var t124 int = t123 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__3, t124)
            var mtmp113 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
            var jp126 int
            switch mtmp113 {
            case 5:
                break Loop_loop122
            default:
                var t129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__3)
                jp126 = t129
                var cur__5 int = jp126
                var t127 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
                var t128 int = t127 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__4, t128)
                continue
            }
        } else {
            break Loop_loop122
        }
    }
    var t121 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__4)
    println__T_int(t121)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv143 *ref_int_x
    var t144 *ref_int_x = ref__Ref_3int(value__207)
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv146 int
    var t147 int = ref_get__Ref_3int(self__208)
    retv146 = t147
    return retv146
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv151 bool
    var t152 bool = self__59 == other__60
    retv151 = t152
    return retv151
}

func println__T_int(value__1 int) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv157 string
    var t158 string = _goml_runtime_core_int_to_string(self__40)
    retv157 = t158
    return retv157
}

func main() {
    main0()
}
