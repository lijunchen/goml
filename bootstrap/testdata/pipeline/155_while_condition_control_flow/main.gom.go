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
    Loop_loop111:
    for {
        var t117 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t118 bool = t117 < 3
        var jp113 bool
        if t118 {
            jp113 = true
        } else {
            jp113 = false
        }
        if jp113 {
            var t114 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            println__T_int(t114)
            var t115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t116 int = t115 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t116)
            continue
        } else {
            break Loop_loop111
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop94:
    for {
        var t102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
        var t103 bool = t102 < 4
        var jp96 bool
        if t103 {
            var t106 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t107 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t106, 1)
            var jp105 bool
            if t107 {
                jp105 = true
            } else {
                var t108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
                var t109 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t108, 3)
                var t110 bool = !t109
                jp105 = t110
            }
            jp96 = jp105
        } else {
            jp96 = false
        }
        if jp96 {
            var t97 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
            var t98 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t99 int = t97 + t98
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__2, t99)
            var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t101 int = t100 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__1, t101)
            continue
        } else {
            break Loop_loop94
        }
    }
    var t79 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t79)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop82:
    for {
        var mtmp71 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
        var jp84 bool
        switch mtmp71 {
        case 0:
            jp84 = true
        case 1:
            var t92 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t93 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t92, 0)
            var jp91 bool
            if t93 {
                jp91 = true
            } else {
                jp91 = false
            }
            jp84 = jp91
        case 2:
            jp84 = true
        default:
            jp84 = false
        }
        if jp84 {
            var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t86 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t87 int = t85 + t86
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__4, t87)
            var t88 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t89 int = t88 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(k__3, t89)
            continue
        } else {
            break Loop_loop82
        }
    }
    var t81 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t81)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv120 *ref_int_x
    var t121 *ref_int_x = ref__Ref_3int(value__209)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv123 int
    var t124 int = ref_get__Ref_3int(self__210)
    retv123 = t124
    return retv123
}

func println__T_int(value__1 int) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv131 bool
    var t132 bool = self__59 == other__60
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int_to_string(self__40)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
