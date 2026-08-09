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

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline254 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline255 closure_env_next_0 = closure_env_next_0{
        cell_0: inline254,
    }
    var inline256 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline255)
    }
    var inline257 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline254,
    }
    var inline258 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline257)
    }
    var inline259 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline256,
        _1: inline258,
    }
    counter__4 = inline259
    var x175 func() int32 = counter__4._0
    var x176 func() struct{} = counter__4._1
    var first__7 int32 = x175()
    var second__8 int32 = x175()
    x176()
    var third__9 int32 = x175()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline247 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline248 closure_env_next_0 = closure_env_next_0{
        cell_0: inline247,
    }
    var inline249 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline248)
    }
    var inline250 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline247,
    }
    var inline251 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline250)
    }
    var inline252 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline249,
        _1: inline251,
    }
    new_counter__10 = inline252
    var x179 func() int32 = new_counter__10._0
    var fourth__12 int32 = x179()
    var t193 string
    var inline245 string = _goml_runtime_core_int32_to_string(first__7)
    t193 = inline245
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline242)
    var t194 string
    var inline240 string = _goml_runtime_core_int32_to_string(second__8)
    t194 = inline240
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline237)
    var t195 string
    var inline235 string = _goml_runtime_core_int32_to_string(third__9)
    t195 = inline235
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline232)
    var t196 string
    var inline230 string = _goml_runtime_core_int32_to_string(fourth__12)
    t196 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline227)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t199 *ref_int32_x = ref__Ref_5int32(value__257)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env185 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env185.cell_0
    var t221 int32
    var inline264 int32 = ref_get__Ref_5int32(cell__0)
    t221 = inline264
    var next__1 int32 = t221 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env186 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env186.cell_0
    var inline266 int32 = 0
    ref_set__Ref_5int32(cell__0, inline266)
    return struct{}{}
}

func main() {
    main0()
}
