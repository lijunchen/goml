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
    var inline214 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline215 closure_env_next_0 = closure_env_next_0{
        cell_0: inline214,
    }
    var inline216 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline214,
    }
    var inline217 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline215)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline216)
        },
    }
    counter__4 = inline217
    var x139 func() int32 = counter__4._0
    var x140 func() struct{} = counter__4._1
    var first__7 int32 = x139()
    var second__8 int32 = x139()
    x140()
    var third__9 int32 = x139()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline209 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline210 closure_env_next_0 = closure_env_next_0{
        cell_0: inline209,
    }
    var inline211 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline209,
    }
    var inline212 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline210)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline211)
        },
    }
    new_counter__10 = inline212
    var x143 func() int32 = new_counter__10._0
    var fourth__12 int32 = x143()
    var t155 string
    var inline207 string = _goml_runtime_core_int32_to_string(first__7)
    t155 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline204)
    var t156 string
    var inline202 string = _goml_runtime_core_int32_to_string(second__8)
    t156 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
    _goml_runtime_core_string_println(inline199)
    var t157 string
    var inline197 string = _goml_runtime_core_int32_to_string(third__9)
    t157 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline194)
    var t158 string
    var inline192 string = _goml_runtime_core_int32_to_string(fourth__12)
    t158 = inline192
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__215 int32) *ref_int32_x {
    var t161 *ref_int32_x = ref__Ref_5int32(value__215)
    return t161
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env149 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env149.cell_0
    var t183 int32
    var inline222 int32 = ref_get__Ref_5int32(cell__0)
    t183 = inline222
    var next__1 int32 = t183 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env150 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env150.cell_0
    var inline224 int32 = 0
    ref_set__Ref_5int32(cell__0, inline224)
    return struct{}{}
}

func main() {
    main0()
}
