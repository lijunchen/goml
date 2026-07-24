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

func array_get__Array_2_15AlwaysDifferent(arr [2]AlwaysDifferent, index int32) AlwaysDifferent {
    return arr[index]
}

type Tuple2_15AlwaysDifferent_5int32 struct {
    _0 AlwaysDifferent
    _1 int32
}

type AlwaysDifferent struct {
    value int32
}

func _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(self__0 AlwaysDifferent, other__1 AlwaysDifferent) bool {
    var retv70 bool
    retv70 = false
    return retv70
}

func main0() struct{} {
    var first__2 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var second__3 AlwaysDifferent = AlwaysDifferent{
        value: 7,
    }
    var t72 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    println__T_bool(t72)
    var t73 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(first__2, second__3)
    var t74 bool = !t73
    println__T_bool(t74)
    var t75 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var left_tuple__4 Tuple2_15AlwaysDifferent_5int32 = Tuple2_15AlwaysDifferent_5int32{
        _0: t75,
        _1: 2,
    }
    var t76 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var right_tuple__5 Tuple2_15AlwaysDifferent_5int32 = Tuple2_15AlwaysDifferent_5int32{
        _0: t76,
        _1: 2,
    }
    var _eq_lhs63 Tuple2_15AlwaysDifferent_5int32 = left_tuple__4
    var _eq_rhs64 Tuple2_15AlwaysDifferent_5int32 = right_tuple__5
    var t91 AlwaysDifferent = _eq_lhs63._0
    var t92 AlwaysDifferent = _eq_rhs64._0
    var t93 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t91, t92)
    var jp78 bool
    if t93 {
        var t94 int32 = _eq_lhs63._1
        var t95 int32 = _eq_rhs64._1
        var t96 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t94, t95)
        jp78 = t96
    } else {
        jp78 = false
    }
    println__T_bool(jp78)
    var t79 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t80 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t79, t80}
    var t81 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t82 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t81, t82}
    var _eq_lhs66 [2]AlwaysDifferent = left_array__6
    var _eq_rhs67 [2]AlwaysDifferent = right_array__7
    var t85 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs66, 0)
    var t86 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs67, 0)
    var t87 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t85, t86)
    var jp84 bool
    if t87 {
        var t88 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_lhs66, 1)
        var t89 AlwaysDifferent = array_get__Array_2_15AlwaysDifferent(_eq_rhs67, 1)
        var t90 bool = _goml_m_trait__impl_i_Eq_i_AlwaysDifferent_i_eq(t88, t89)
        jp84 = t90
    } else {
        jp84 = false
    }
    println__T_bool(jp84)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv101 bool
    var t102 bool = self__61 == other__62
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv104 string
    var t105 string = _goml_runtime_core_bool_to_string(self__36)
    retv104 = t105
    return retv104
}

func main() {
    main0()
}
