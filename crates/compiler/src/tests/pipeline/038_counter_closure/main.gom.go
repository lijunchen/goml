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
    var retv77 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var cell__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    var t78 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(next__2)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(reset__3)
        },
    }
    retv77 = t78
    return retv77
}

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp63 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = counter__4
    var x64 func() int32 = mtmp63._0
    var x65 func() struct{} = mtmp63._1
    var reset__6 func() struct{} = x65
    var next__5 func() int32 = x64
    var first__7 int32 = next__5()
    var second__8 int32 = next__5()
    reset__6()
    var third__9 int32 = next__5()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp67 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = new_counter__10
    var x68 func() int32 = mtmp67._0
    var new_next__11 func() int32 = x68
    var fourth__12 int32 = new_next__11()
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__7)
    println__T_string(t80)
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__8)
    println__T_string(t81)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__9)
    println__T_string(t82)
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(fourth__12)
    println__T_string(t83)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv85 *ref_int32_x
    var t86 *ref_int32_x = ref__Ref_5int32(value__204)
    retv85 = t86
    return retv85
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv88 int32
    var t89 int32 = ref_get__Ref_5int32(self__205)
    retv88 = t89
    return retv88
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__5)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv99 string
    retv99 = self__37
    return retv99
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env74 closure_env_next_0) int32 {
    var retv107 int32
    var cell__0 *ref_int32_x = env74.cell_0
    var t108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var next__1 int32 = t108 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, next__1)
    retv107 = next__1
    return retv107
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env75 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env75.cell_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, 0)
    return struct{}{}
}

func main() {
    main0()
}
