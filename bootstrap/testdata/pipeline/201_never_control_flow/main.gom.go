package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
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

func choose(flag__0 bool) int32 {
    var retv71 int32
    var jp73 int32
    if flag__0 {
        retv71 = 10
        return retv71
    } else {
        jp73 = 20
        var value__1 int32 = jp73
        var t74 int32 = value__1 + 1
        retv71 = t74
        return retv71
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop77:
    for {
        var t78 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
        var t79 bool = t78 < 2
        if t79 {
            var t80 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t81 int = t80 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(count__2, t81)
            var t85 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t86 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t85, 1)
            var jp83 int
            if t86 {
                continue
            } else {
                jp83 = 7
                var value__3 int = jp83
                println__T_int(value__3)
                continue
            }
        } else {
            break Loop_loop77
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp91 int
        if stop__4 {
            return struct{}{}
        } else {
            jp91 = 9
            var value__5 int = jp91
            println__T_int(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t93 int32 = choose(false)
    println__T_int32(t93)
    var t94 int32 = choose(true)
    println__T_int32(t94)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv97 *ref_int_x
    var t98 *ref_int_x = ref__Ref_3int(value__209)
    retv97 = t98
    return retv97
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv100 int
    var t101 int = ref_get__Ref_3int(self__210)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv105 bool
    var t106 bool = self__59 == other__60
    retv105 = t106
    return retv105
}

func println__T_int(value__1 int) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv114 string
    var t115 string = _goml_runtime_core_int_to_string(self__40)
    retv114 = t115
    return retv114
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int32_to_string(self__43)
    retv117 = t118
    return retv117
}

func main() {
    main0()
}
