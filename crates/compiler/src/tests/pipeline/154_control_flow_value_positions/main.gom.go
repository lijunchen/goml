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
    var sum__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop80:
    for {
        var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t82 bool = t81 < 5
        if t82 {
            var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t84 int32 = t83 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t84)
            var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t90 bool = t89 == 3
            var jp86 int32
            if t90 {
                continue
            } else {
                var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                jp86 = t91
                var cur__2 int32 = jp86
                var t87 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                var t88 int32 = t87 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t88)
                continue
            }
        } else {
            break Loop_loop80
        }
    }
    var t69 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t69)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop72:
    for {
        if true {
            var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t74 int32 = t73 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t74)
            var mtmp63 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var jp76 int32
            switch mtmp63 {
            case 5:
                break Loop_loop72
            default:
                var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
                jp76 = t79
                var cur__5 int32 = jp76
                var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
                var t78 int32 = t77 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__4, t78)
                continue
            }
        } else {
            break Loop_loop72
        }
    }
    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
    println__T_int32(t71)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv93 *ref_int32_x
    var t94 *ref_int32_x = ref__Ref_5int32(value__200)
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv96 int32
    var t97 int32 = ref_get__Ref_5int32(self__201)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int32_to_string(self__38)
    retv104 = t105
    return retv104
}

func main() {
    main0()
}
