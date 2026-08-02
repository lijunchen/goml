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
    var inline233 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline234 closure_env_next_0 = closure_env_next_0{
        cell_0: inline233,
    }
    var inline235 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline233,
    }
    var inline236 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline234)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline235)
        },
    }
    counter__4 = inline236
    var x158 func() int32 = counter__4._0
    var x159 func() struct{} = counter__4._1
    var first__7 int32 = x158()
    var second__8 int32 = x158()
    x159()
    var third__9 int32 = x158()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline228 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline229 closure_env_next_0 = closure_env_next_0{
        cell_0: inline228,
    }
    var inline230 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline228,
    }
    var inline231 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline229)
        },
        _1: func() struct{} {
            return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline230)
        },
    }
    new_counter__10 = inline231
    var x162 func() int32 = new_counter__10._0
    var fourth__12 int32 = x162()
    var t174 string
    var inline226 string = _goml_runtime_core_int32_to_string(first__7)
    t174 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline223)
    var t175 string
    var inline221 string = _goml_runtime_core_int32_to_string(second__8)
    t175 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline218)
    var t176 string
    var inline216 string = _goml_runtime_core_int32_to_string(third__9)
    t176 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline213)
    var t177 string
    var inline211 string = _goml_runtime_core_int32_to_string(fourth__12)
    t177 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t180 *ref_int32_x = ref__Ref_5int32(value__207)
    return t180
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env168 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env168.cell_0
    var t202 int32
    var inline241 int32 = ref_get__Ref_5int32(cell__0)
    t202 = inline241
    var next__1 int32 = t202 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env169 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env169.cell_0
    var inline243 int32 = 0
    ref_set__Ref_5int32(cell__0, inline243)
    return struct{}{}
}

func main() {
    main0()
}
