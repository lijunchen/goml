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
    var retv115 int32
    var jp117 int32
    if flag__0 {
        retv115 = 10
        return retv115
    } else {
        jp117 = 20
        var value__1 int32 = jp117
        var t118 int32 = value__1 + 1
        retv115 = t118
        return retv115
    }
}

func continue_branch() struct{} {
    var count__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop121:
    for {
        var t122 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
        var t123 bool = t122 < 2
        if t123 {
            var t124 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t125 int = t124 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(count__2, t125)
            var t129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(count__2)
            var t130 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t129, 1)
            var jp127 int
            if t130 {
                continue
            } else {
                jp127 = 7
                var value__3 int = jp127
                println__T_int(value__3)
                continue
            }
        } else {
            break Loop_loop121
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    if true {
        var jp135 int
        if stop__4 {
            return struct{}{}
        } else {
            jp135 = 9
            var value__5 int = jp135
            println__T_int(value__5)
            return struct{}{}
        }
    } else {
        return struct{}{}
    }
}

func main0() struct{} {
    var t137 int32 = choose(false)
    println__T_int32(t137)
    var t138 int32 = choose(true)
    println__T_int32(t138)
    continue_branch()
    break_branch(false)
    break_branch(true)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv141 *ref_int_x
    var t142 *ref_int_x = ref__Ref_3int(value__207)
    retv141 = t142
    return retv141
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv144 int
    var t145 int = ref_get__Ref_3int(self__208)
    retv144 = t145
    return retv144
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv149 bool
    var t150 bool = self__59 == other__60
    retv149 = t150
    return retv149
}

func println__T_int(value__1 int) struct{} {
    var t152 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t152)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t155 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t155)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv158 string
    var t159 string = _goml_runtime_core_int_to_string(self__40)
    retv158 = t159
    return retv158
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv161 string
    var t162 string = _goml_runtime_core_int32_to_string(self__43)
    retv161 = t162
    return retv161
}

func main() {
    main0()
}
