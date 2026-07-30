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
    var retv117 bool
    retv117 = false
    return retv117
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t119 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t119)
    var t120 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t121 bool = !t120
    println__T_bool(t121)
    var t122 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var left_tuple__4 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t122,
        _1: 2,
    }
    var t123 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t123,
        _1: 2,
    }
    var _eq_lhs110 Tuple2_15AlwaysDifferent_3int = left_tuple__4
    var _eq_rhs111 Tuple2_15AlwaysDifferent_3int = right_tuple__5
    var t138 AlwaysDifferent = _eq_lhs110._0
    var t139 AlwaysDifferent = _eq_rhs111._0
    var t140 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t138, t139)
    var jp125 bool
    if t140 {
        var t141 int = _eq_lhs110._1
        var t142 int = _eq_rhs111._1
        var t143 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t141, t142)
        jp125 = t143
    } else {
        jp125 = false
    }
    println__T_bool(jp125)
    var t126 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t127 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t126, t127}
    var t128 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t129 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t128, t129}
    var _eq_lhs113 [2]AlwaysDifferent = left_array__6
    var _eq_rhs114 [2]AlwaysDifferent = right_array__7
    var t132 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs113, 0)
    var t133 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs114, 0)
    var t134 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t132, t133)
    var jp131 bool
    if t134 {
        var t135 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs113, 1)
        var t136 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs114, 1)
        var t137 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t135, t136)
        jp131 = t137
    } else {
        jp131 = false
    }
    println__T_bool(jp131)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv148 bool
    var t149 bool = self__59 == other__60
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv151 string
    var t152 string = _goml_runtime_core_bool_to_string(self__37)
    retv151 = t152
    return retv151
}

func main() {
    main0()
}
