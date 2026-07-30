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
    var retv77 bool
    retv77 = false
    return retv77
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t79 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t79)
    var t80 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t81 bool = !t80
    println__T_bool(t81)
    var t82 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var left_tuple__4 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t82,
        _1: 2,
    }
    var t83 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t83,
        _1: 2,
    }
    var _eq_lhs70 Tuple2_15AlwaysDifferent_3int = left_tuple__4
    var _eq_rhs71 Tuple2_15AlwaysDifferent_3int = right_tuple__5
    var t98 AlwaysDifferent = _eq_lhs70._0
    var t99 AlwaysDifferent = _eq_rhs71._0
    var t100 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t98, t99)
    var jp85 bool
    if t100 {
        var t101 int = _eq_lhs70._1
        var t102 int = _eq_rhs71._1
        var t103 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t101, t102)
        jp85 = t103
    } else {
        jp85 = false
    }
    println__T_bool(jp85)
    var t86 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t87 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t86, t87}
    var t88 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t89 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t88, t89}
    var _eq_lhs73 [2]AlwaysDifferent = left_array__6
    var _eq_rhs74 [2]AlwaysDifferent = right_array__7
    var t92 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs73, 0)
    var t93 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs74, 0)
    var t94 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t92, t93)
    var jp91 bool
    if t94 {
        var t95 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs73, 1)
        var t96 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs74, 1)
        var t97 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t95, t96)
        jp91 = t97
    } else {
        jp91 = false
    }
    println__T_bool(jp91)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv108 bool
    var t109 bool = self__59 == other__60
    retv108 = t109
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv111 string
    var t112 string = _goml_runtime_core_bool_to_string(self__37)
    retv111 = t112
    return retv111
}

func main() {
    main0()
}
