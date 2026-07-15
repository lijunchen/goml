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
    Loop_loop44:
    for {
        var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t46 bool = t45 < 5
        if t46 {
            var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t48 int32 = t47 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t48)
            var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t54 bool = t53 == 3
            var jp50 int32
            if t54 {
                continue
            } else {
                var t55 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
                jp50 = t55
                var cur__2 int32 = jp50
                var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
                var t52 int32 = t51 + cur__2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__1, t52)
                continue
            }
        } else {
            break Loop_loop44
        }
    }
    var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__1)
    println__T_int32(t33)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop36:
    for {
        if true {
            var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t38 int32 = t37 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t38)
            var mtmp27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var jp40 int32
            switch mtmp27 {
            case 5:
                break Loop_loop36
            default:
                var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
                jp40 = t43
                var cur__5 int32 = jp40
                var t41 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
                var t42 int32 = t41 + cur__5
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__4, t42)
                continue
            }
        } else {
            break Loop_loop36
        }
    }
    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__4)
    println__T_int32(t35)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv57 *ref_int32_x
    var t58 *ref_int32_x = ref__Ref_5int32(value__140)
    retv57 = t58
    return retv57
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv60 int32
    var t61 int32 = ref_get__Ref_5int32(self__141)
    retv60 = t61
    return retv60
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv68 string
    var t69 string = _goml_runtime_core_int32_to_string(self__13)
    retv68 = t69
    return retv68
}

func main() {
    main0()
}
