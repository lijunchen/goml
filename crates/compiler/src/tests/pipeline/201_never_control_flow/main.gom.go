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

func choose(flag__0 bool) int32 {
    var retv68 int32
    var jp70 int32
    if flag__0 {
        retv68 = 10
        return retv68
    } else {
        jp70 = 20
        var value__1 int32 = jp70
        var t71 int32 = value__1 + 1
        retv68 = t71
        return retv68
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop74:
    for {
        var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
        var t76 bool = t75 < 2
        if t76 {
            var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
            var t78 int32 = t77 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__2, t78)
            var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
            var t83 bool = t82 == 1
            var jp80 int32
            if t83 {
                continue
            } else {
                jp80 = 7
                var value__3 int32 = jp80
                println__T_int32(value__3)
                continue
            }
        } else {
            break Loop_loop74
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp88 int32
        if stop__4 {
            return struct{}{}
        } else {
            jp88 = 9
            var value__5 int32 = jp88
            println__T_int32(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t90 int32 = choose(false)
    println__T_int32(t90)
    var t91 int32 = choose(true)
    println__T_int32(t91)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv94 *ref_int32_x
    var t95 *ref_int32_x = ref__Ref_5int32(value__204)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv97 int32
    var t98 int32 = ref_get__Ref_5int32(self__205)
    retv97 = t98
    return retv97
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv105 string
    var t106 string = _goml_runtime_core_int32_to_string(self__41)
    retv105 = t106
    return retv105
}

func main() {
    main0()
}
