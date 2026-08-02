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
    var cell__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    var t172 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(next__2)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(reset__3)
        },
    }
    return t172
}

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var x158 func() int32 = counter__4._0
    var x159 func() struct{} = counter__4._1
    var first__7 int32 = x158()
    var second__8 int32 = x158()
    x159()
    var third__9 int32 = x158()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var x162 func() int32 = new_counter__10._0
    var fourth__12 int32 = x162()
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__7)
    println__T_string(t174)
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__8)
    println__T_string(t175)
    var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__9)
    println__T_string(t176)
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(fourth__12)
    println__T_string(t177)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t180 *ref_int32_x = ref__Ref_5int32(value__207)
    return t180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t183 int32 = ref_get__Ref_5int32(self__208)
    return t183
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t191 string = _goml_runtime_core_int32_to_string(self__6)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env168 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env168.cell_0
    var t202 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(cell__0)
    var next__1 int32 = t202 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env169 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env169.cell_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(cell__0, 0)
    return struct{}{}
}

func main() {
    main0()
}
