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
    var inline493 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline494 closure_env_next_0 = closure_env_next_0{
        cell_0: inline493,
    }
    var inline495 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline494)
    }
    var inline496 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline493,
    }
    var inline497 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline496)
    }
    var inline498 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline495,
        _1: inline497,
    }
    counter__4 = inline498
    var x414 func() int32 = counter__4._0
    var x415 func() struct{} = counter__4._1
    var first__7 int32 = x414()
    var second__8 int32 = x414()
    x415()
    var third__9 int32 = x414()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline486 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline487 closure_env_next_0 = closure_env_next_0{
        cell_0: inline486,
    }
    var inline488 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline487)
    }
    var inline489 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline486,
    }
    var inline490 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline489)
    }
    var inline491 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline488,
        _1: inline490,
    }
    new_counter__10 = inline491
    var x418 func() int32 = new_counter__10._0
    var fourth__12 int32 = x418()
    var t432 string
    var inline484 string = _goml_runtime_core_int32_to_string(first__7)
    t432 = inline484
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline481)
    var t433 string
    var inline479 string = _goml_runtime_core_int32_to_string(second__8)
    t433 = inline479
    var inline476 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline476)
    var t434 string
    var inline474 string = _goml_runtime_core_int32_to_string(third__9)
    t434 = inline474
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline471)
    var t435 string
    var inline469 string = _goml_runtime_core_int32_to_string(fourth__12)
    t435 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline466)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__431 int32) *ref_int32_x {
    var t438 *ref_int32_x = ref__Ref_5int32(value__431)
    return t438
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env424 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env424.cell_0
    var t460 int32
    var inline503 int32 = ref_get__Ref_5int32(cell__0)
    t460 = inline503
    var next__1 int32 = t460 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env425 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env425.cell_0
    var inline505 int32 = 0
    ref_set__Ref_5int32(cell__0, inline505)
    return struct{}{}
}

func main() {
    main0()
}
