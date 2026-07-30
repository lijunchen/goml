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

type Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit struct {
    _0 func() int32
    _1 func() struct{}
}

type closure_env_next_0 struct {
    cell_0 *ref_int32_x
}

type closure_env_reset_1 struct {
    cell_0 *ref_int32_x
}

func make_counter() Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit {
    var retv124 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var cell__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    var t125 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(next__2)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(reset__3)
        },
    }
    retv124 = t125
    return retv124
}

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp110 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = counter__4
    var x111 func() int32 = mtmp110._0
    var x112 func() struct{} = mtmp110._1
    var reset__6 func() struct{} = x112
    var next__5 func() int32 = x111
    var first__7 int32 = next__5()
    var second__8 int32 = next__5()
    reset__6()
    var third__9 int32 = next__5()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp114 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = new_counter__10
    var x115 func() int32 = mtmp114._0
    var new_next__11 func() int32 = x115
    var fourth__12 int32 = new_next__11()
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__7)
    println__T_string(t127)
    var t128 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__8)
    println__T_string(t128)
    var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__9)
    println__T_string(t129)
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(fourth__12)
    println__T_string(t130)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv132 *ref_int32_x
    var t133 *ref_int32_x = ref__Ref_5int32(value__207)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv135 int32
    var t136 int32 = ref_get__Ref_5int32(self__208)
    retv135 = t136
    return retv135
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_int32_to_string(self__6)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv146 string
    retv146 = self__38
    return retv146
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env121 closure_env_next_0) int32 {
    var retv154 int32
    var cell__0 *ref_int32_x = env121.cell_0
    var t155 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var next__1 int32 = t155 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, next__1)
    retv154 = next__1
    return retv154
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env122 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env122.cell_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, 0)
    return struct{}{}
}

func main() {
    main0()
}
