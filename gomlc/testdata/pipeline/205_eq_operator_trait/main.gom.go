package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_15AlwaysDifferent(arr [2]AlwaysDifferent, index int) AlwaysDifferent {
    return arr[index]
}

type Tuple2_15AlwaysDifferent_3int struct {
    _0 AlwaysDifferent
    _1 int
}

type AlwaysDifferent struct {
    value int32
}

func _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(self__0 AlwaysDifferent, other__1 AlwaysDifferent) bool {
    return false
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t166 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t166)
    var t167 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t168 bool = !t167
    println__T_bool(t168)
    var t169 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t170 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t187 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t169, t170)
    var jp172 bool
    if t187 {
        var t188 int = 2
        var t189 int = 2
        var t190 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t188, t189)
        jp172 = t190
    } else {
        jp172 = false
    }
    println__T_bool(jp172)
    var t173 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t174 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t173, t174}
    var t175 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t176 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t175, t176}
    var t179 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    var t180 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t181 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t179, t180)
    var jp178 bool
    if t181 {
        var t182 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        var t183 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        var t184 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t182, t183)
        jp178 = t184
    } else {
        jp178 = false
    }
    println__T_bool(jp178)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t196 bool = self__59 == other__60
    return t196
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t199 string = _goml_runtime_core_bool_to_string(self__37)
    return t199
}

func main() {
    main0()
}
