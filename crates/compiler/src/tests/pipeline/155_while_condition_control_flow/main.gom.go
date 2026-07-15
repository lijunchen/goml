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
    Loop_loop68:
    for {
        var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
        var t75 bool = t74 < 3
        var jp70 bool
        if t75 {
            jp70 = true
        } else {
            jp70 = false
        }
        if jp70 {
            var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            println__T_int32(t71)
            var t72 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__0)
            var t73 int32 = t72 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__0, t73)
            continue
        } else {
            break Loop_loop68
        }
    }
    var j__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop52:
    for {
        var t60 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
        var t61 bool = t60 < 4
        var jp54 bool
        if t61 {
            var t64 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t65 bool = t64 == 1
            var jp63 bool
            if t65 {
                jp63 = true
            } else {
                var t66 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
                var t67 bool = t66 != 3
                jp63 = t67
            }
            jp54 = jp63
        } else {
            jp54 = false
        }
        if jp54 {
            var t55 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
            var t56 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t57 int32 = t55 + t56
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__2, t57)
            var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__1)
            var t59 int32 = t58 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__1, t59)
            continue
        } else {
            break Loop_loop52
        }
    }
    var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__2)
    println__T_int32(t37)
    var k__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var sum__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop40:
    for {
        var mtmp29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
        var jp42 bool
        switch mtmp29 {
        case 0:
            jp42 = true
        case 1:
            var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t51 bool = t50 == 0
            var jp49 bool
            if t51 {
                jp49 = true
            } else {
                jp49 = false
            }
            jp42 = jp49
        case 2:
            jp42 = true
        default:
            jp42 = false
        }
        if jp42 {
            var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
            var t44 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t45 int32 = t43 + t44
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__4, t45)
            var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(k__3)
            var t47 int32 = t46 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(k__3, t47)
            continue
        } else {
            break Loop_loop40
        }
    }
    var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__4)
    println__T_int32(t39)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv77 *ref_int32_x
    var t78 *ref_int32_x = ref__Ref_5int32(value__140)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv80 int32
    var t81 int32 = ref_get__Ref_5int32(self__141)
    retv80 = t81
    return retv80
}

func println__T_int32(value__1 int32) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t83)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv88 string
    var t89 string = _goml_runtime_core_int32_to_string(self__13)
    retv88 = t89
    return retv88
}

func main() {
    main0()
}
