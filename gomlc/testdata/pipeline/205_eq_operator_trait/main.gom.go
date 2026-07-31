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
    var retv161 bool
    retv161 = false
    return retv161
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t163 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t163)
    var t164 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t165 bool = !t164
    println__T_bool(t165)
    var t166 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var left_tuple__4 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t166,
        _1: 2,
    }
    var t167 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t167,
        _1: 2,
    }
    var _eq_lhs154 Tuple2_15AlwaysDifferent_3int = left_tuple__4
    var _eq_rhs155 Tuple2_15AlwaysDifferent_3int = right_tuple__5
    var t182 AlwaysDifferent = _eq_lhs154._0
    var t183 AlwaysDifferent = _eq_rhs155._0
    var t184 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t182, t183)
    var jp169 bool
    if t184 {
        var t185 int = _eq_lhs154._1
        var t186 int = _eq_rhs155._1
        var t187 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t185, t186)
        jp169 = t187
    } else {
        jp169 = false
    }
    println__T_bool(jp169)
    var t170 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t171 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t170, t171}
    var t172 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t173 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t172, t173}
    var _eq_lhs157 [2]AlwaysDifferent = left_array__6
    var _eq_rhs158 [2]AlwaysDifferent = right_array__7
    var t176 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs157, 0)
    var t177 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs158, 0)
    var t178 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t176, t177)
    var jp175 bool
    if t178 {
        var t179 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs157, 1)
        var t180 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs158, 1)
        var t181 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t179, t180)
        jp175 = t181
    } else {
        jp175 = false
    }
    println__T_bool(jp175)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv192 bool
    var t193 bool = self__59 == other__60
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv195 string
    var t196 string = _goml_runtime_core_bool_to_string(self__37)
    retv195 = t196
    return retv195
}

func main() {
    main0()
}
