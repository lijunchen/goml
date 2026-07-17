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
    Loop_loop104:
    for {
        var t110 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t111 bool = t110 < 3
        var jp106 bool
        if t111 {
            jp106 = true
        } else {
            jp106 = false
        }
        if jp106 {
            var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            println__T_int32(t107)
            var t108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t109 int32 = t108 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t109)
            continue
        } else {
            break Loop_loop104
        }
    }
    var j__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop88:
    for {
        var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
        var t97 bool = t96 < 4
        var jp90 bool
        if t97 {
            var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t101 bool = t100 == 1
            var jp99 bool
            if t101 {
                jp99 = true
            } else {
                var t102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
                var t103 bool = t102 != 3
                jp99 = t103
            }
            jp90 = jp99
        } else {
            jp90 = false
        }
        if jp90 {
            var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
            var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t93 int32 = t91 + t92
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t93)
            var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t95 int32 = t94 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__1, t95)
            continue
        } else {
            break Loop_loop88
        }
    }
    var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    println__T_int32(t73)
    var k__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var sum__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop76:
    for {
        var mtmp65 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
        var jp78 bool
        switch mtmp65 {
        case 0:
            jp78 = true
        case 1:
            var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t87 bool = t86 == 0
            var jp85 bool
            if t87 {
                jp85 = true
            } else {
                jp85 = false
            }
            jp78 = jp85
        case 2:
            jp78 = true
        default:
            jp78 = false
        }
        if jp78 {
            var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t80 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t81 int32 = t79 + t80
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__4, t81)
            var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t83 int32 = t82 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(k__3, t83)
            continue
        } else {
            break Loop_loop76
        }
    }
    var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
    println__T_int32(t75)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv113 *ref_int32_x
    var t114 *ref_int32_x = ref__Ref_5int32(value__201)
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv116 int32
    var t117 int32 = ref_get__Ref_5int32(self__202)
    retv116 = t117
    return retv116
}

func println__T_int32(value__1 int32) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv124 string
    var t125 string = _goml_runtime_core_int32_to_string(self__38)
    retv124 = t125
    return retv124
}

func main() {
    main0()
}
