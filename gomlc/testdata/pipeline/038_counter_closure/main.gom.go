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
    var inline264 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline265 closure_env_next_0 = closure_env_next_0{
        cell_0: inline264,
    }
    var inline266 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline265)
    }
    var inline267 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline264,
    }
    var inline268 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline267)
    }
    var inline269 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline266,
        _1: inline268,
    }
    counter__4 = inline269
    var x185 func() int32 = counter__4._0
    var x186 func() struct{} = counter__4._1
    var first__7 int32 = x185()
    var second__8 int32 = x185()
    x186()
    var third__9 int32 = x185()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline257 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline258 closure_env_next_0 = closure_env_next_0{
        cell_0: inline257,
    }
    var inline259 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline258)
    }
    var inline260 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline257,
    }
    var inline261 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline260)
    }
    var inline262 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline259,
        _1: inline261,
    }
    new_counter__10 = inline262
    var x189 func() int32 = new_counter__10._0
    var fourth__12 int32 = x189()
    var t203 string
    var inline255 string = _goml_runtime_core_int32_to_string(first__7)
    t203 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline252)
    var t204 string
    var inline250 string = _goml_runtime_core_int32_to_string(second__8)
    t204 = inline250
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline247)
    var t205 string
    var inline245 string = _goml_runtime_core_int32_to_string(third__9)
    t205 = inline245
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline242)
    var t206 string
    var inline240 string = _goml_runtime_core_int32_to_string(fourth__12)
    t206 = inline240
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t209 *ref_int32_x = ref__Ref_5int32(value__270)
    return t209
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env195 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env195.cell_0
    var t231 int32
    var inline274 int32 = ref_get__Ref_5int32(cell__0)
    t231 = inline274
    var next__1 int32 = t231 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env196 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env196.cell_0
    var inline276 int32 = 0
    ref_set__Ref_5int32(cell__0, inline276)
    return struct{}{}
}

func main() {
    main0()
}
