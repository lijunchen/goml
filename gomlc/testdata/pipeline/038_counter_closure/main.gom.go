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

type Ordering int32

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline490 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline491 closure_env_next_0 = closure_env_next_0{
        cell_0: inline490,
    }
    var inline492 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline491)
    }
    var inline493 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline490,
    }
    var inline494 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline493)
    }
    var inline495 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline492,
        _1: inline494,
    }
    counter__4 = inline495
    var x411 func() int32 = counter__4._0
    var x412 func() struct{} = counter__4._1
    var first__7 int32 = x411()
    var second__8 int32 = x411()
    x412()
    var third__9 int32 = x411()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline483 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline484 closure_env_next_0 = closure_env_next_0{
        cell_0: inline483,
    }
    var inline485 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline484)
    }
    var inline486 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline483,
    }
    var inline487 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline486)
    }
    var inline488 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline485,
        _1: inline487,
    }
    new_counter__10 = inline488
    var x415 func() int32 = new_counter__10._0
    var fourth__12 int32 = x415()
    var t429 string
    var inline481 string = _goml_runtime_core_int32_to_string(first__7)
    t429 = inline481
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline478)
    var t430 string
    var inline476 string = _goml_runtime_core_int32_to_string(second__8)
    t430 = inline476
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline473)
    var t431 string
    var inline471 string = _goml_runtime_core_int32_to_string(third__9)
    t431 = inline471
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline468)
    var t432 string
    var inline466 string = _goml_runtime_core_int32_to_string(fourth__12)
    t432 = inline466
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline463)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t435 *ref_int32_x = ref__Ref_5int32(value__431)
    return t435
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env421 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env421.cell_0
    var t457 int32
    var inline500 int32 = ref_get__Ref_5int32(cell__0)
    t457 = inline500
    var next__1 int32 = t457 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env422 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env422.cell_0
    var inline502 int32 = 0
    ref_set__Ref_5int32(cell__0, inline502)
    return struct{}{}
}

func main() {
    main0()
}
