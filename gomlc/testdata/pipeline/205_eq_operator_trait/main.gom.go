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
    var retv164 bool
    retv164 = false
    return retv164
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
    var left_tuple__4 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t169,
        _1: 2,
    }
    var t170 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t170,
        _1: 2,
    }
    var _eq_lhs157 Tuple2_15AlwaysDifferent_3int = left_tuple__4
    var _eq_rhs158 Tuple2_15AlwaysDifferent_3int = right_tuple__5
    var t185 AlwaysDifferent = _eq_lhs157._0
    var t186 AlwaysDifferent = _eq_rhs158._0
    var t187 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t185, t186)
    var jp172 bool
    if t187 {
        var t188 int = _eq_lhs157._1
        var t189 int = _eq_rhs158._1
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
    var _eq_lhs160 [2]AlwaysDifferent = left_array__6
    var _eq_rhs161 [2]AlwaysDifferent = right_array__7
    var t179 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs160, 0)
    var t180 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs161, 0)
    var t181 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t179, t180)
    var jp178 bool
    if t181 {
        var t182 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs160, 1)
        var t183 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs161, 1)
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
    var retv195 bool
    var t196 bool = self__59 == other__60
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv198 string
    var t199 string = _goml_runtime_core_bool_to_string(self__37)
    retv198 = t199
    return retv198
}

func main() {
    main0()
}
