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
    var retv75 int32
    var jp77 int32
    if flag__0 {
        retv75 = 10
        return retv75
    } else {
        jp77 = 20
        var value__1 int32 = jp77
        var t78 int32 = value__1 + 1
        retv75 = t78
        return retv75
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop81:
    for {
        var t82 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
        var t83 bool = t82 < 2
        if t83 {
            var t84 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t85 int = t84 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(count__2, t85)
            var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t90 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t89, 1)
            var jp87 int
            if t90 {
                continue
            } else {
                jp87 = 7
                var value__3 int = jp87
                println__T_int(value__3)
                continue
            }
        } else {
            break Loop_loop81
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp95 int
        if stop__4 {
            return struct{}{}
        } else {
            jp95 = 9
            var value__5 int = jp95
            println__T_int(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t97 int32 = choose(false)
    println__T_int32(t97)
    var t98 int32 = choose(true)
    println__T_int32(t98)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv101 *ref_int_x
    var t102 *ref_int_x = ref__Ref_3int(value__207)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv104 int
    var t105 int = ref_get__Ref_3int(self__208)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv109 bool
    var t110 bool = self__59 == other__60
    retv109 = t110
    return retv109
}

func println__T_int(value__1 int) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int_to_string(self__40)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int32_to_string(self__43)
    retv121 = t122
    return retv121
}

func main() {
    main0()
}
