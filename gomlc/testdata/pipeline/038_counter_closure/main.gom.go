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
    var inline269 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline270 closure_env_next_0 = closure_env_next_0{
        cell_0: inline269,
    }
    var inline271 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline270)
    }
    var inline272 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline269,
    }
    var inline273 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline272)
    }
    var inline274 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline271,
        _1: inline273,
    }
    counter__4 = inline274
    var x190 func() int32 = counter__4._0
    var x191 func() struct{} = counter__4._1
    var first__7 int32 = x190()
    var second__8 int32 = x190()
    x191()
    var third__9 int32 = x190()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline262 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline263 closure_env_next_0 = closure_env_next_0{
        cell_0: inline262,
    }
    var inline264 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline263)
    }
    var inline265 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline262,
    }
    var inline266 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline265)
    }
    var inline267 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline264,
        _1: inline266,
    }
    new_counter__10 = inline267
    var x194 func() int32 = new_counter__10._0
    var fourth__12 int32 = x194()
    var t208 string
    var inline260 string = _goml_runtime_core_int32_to_string(first__7)
    t208 = inline260
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline257)
    var t209 string
    var inline255 string = _goml_runtime_core_int32_to_string(second__8)
    t209 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline252)
    var t210 string
    var inline250 string = _goml_runtime_core_int32_to_string(third__9)
    t210 = inline250
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline247)
    var t211 string
    var inline245 string = _goml_runtime_core_int32_to_string(fourth__12)
    t211 = inline245
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t214 *ref_int32_x = ref__Ref_5int32(value__273)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env200 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env200.cell_0
    var t236 int32
    var inline279 int32 = ref_get__Ref_5int32(cell__0)
    t236 = inline279
    var next__1 int32 = t236 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env201 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env201.cell_0
    var inline281 int32 = 0
    ref_set__Ref_5int32(cell__0, inline281)
    return struct{}{}
}

func main() {
    main0()
}
