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
    var retv73 bool
    retv73 = false
    return retv73
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t75 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t75)
    var t76 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t77 bool = !t76
    println__T_bool(t77)
    var t78 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var left_tuple__4 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t78,
        _1: 2,
    }
    var t79 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_3int = Tuple2_15AlwaysDifferent_3int{
        _0: t79,
        _1: 2,
    }
    var _eq_lhs66 Tuple2_15AlwaysDifferent_3int = left_tuple__4
    var _eq_rhs67 Tuple2_15AlwaysDifferent_3int = right_tuple__5
    var t94 AlwaysDifferent = _eq_lhs66._0
    var t95 AlwaysDifferent = _eq_rhs67._0
    var t96 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t94, t95)
    var jp81 bool
    if t96 {
        var t97 int = _eq_lhs66._1
        var t98 int = _eq_rhs67._1
        var t99 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t97, t98)
        jp81 = t99
    } else {
        jp81 = false
    }
    println__T_bool(jp81)
    var t82 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t83 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t82, t83}
    var t84 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t85 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t84, t85}
    var _eq_lhs69 [2]AlwaysDifferent = left_array__6
    var _eq_rhs70 [2]AlwaysDifferent = right_array__7
    var t88 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs69, 0)
    var t89 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs70, 0)
    var t90 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t88, t89)
    var jp87 bool
    if t90 {
        var t91 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs69, 1)
        var t92 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs70, 1)
        var t93 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t91, t92)
        jp87 = t93
    } else {
        jp87 = false
    }
    println__T_bool(jp87)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv104 bool
    var t105 bool = self__59 == other__60
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv107 string
    var t108 string = _goml_runtime_core_bool_to_string(self__37)
    retv107 = t108
    return retv107
}

func main() {
    main0()
}
