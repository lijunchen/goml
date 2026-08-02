package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type closure_env_run_0 struct {}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func early_return() int {
    var defer_return155 int = 7
    println__T_string("return:inner")
    println__T_string("return:outer")
    return defer_return155
}

func maybe(value__0 Option__int) Option__int {
    var jp210 int
    switch value__0.(type) {
    case None:
        var defer_return164 Option__int = None{}
        println__T_string("try:cleanup")
        return defer_return164
    case Some:
        var x163 int = value__0.(Some)._0
        jp210 = x163
        var defer_result166 Option__int = Some{
            _0: jp210,
        }
        println__T_string("try:cleanup")
        return defer_result166
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop213:
    for {
        var t214 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__2)
        var t215 bool = t214 < 3
        if t215 {
            var current__3 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__2)
            var t216 int = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__2, t216)
            var t220 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(current__3, 0)
            if t220 {
                var t221 string = _goml_m_inherent_i_int_i_int_i_to__string(current__3)
                var t222 string = "loop:" + t221
                println__T_string(t222)
                continue
            } else {
                var t224 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(current__3, 1)
                if t224 {
                    var t225 string = _goml_m_inherent_i_int_i_int_i_to__string(current__3)
                    var t226 string = "loop:" + t225
                    println__T_string(t226)
                    break Loop_loop213
                } else {
                    var t218 string = _goml_m_inherent_i_int_i_int_i_to__string(current__3)
                    var t219 string = "loop:" + t218
                    println__T_string(t219)
                    continue
                }
            }
        } else {
            break Loop_loop213
        }
    }
    return struct{}{}
}

func observed_at_exit() struct{} {
    var value__4 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(value__4, "after")
    var t228 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(value__4)
    var t229 string = "observed:" + t228
    println__T_string(t229)
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x179 int = value__5.(Some)._0
        var x182 int = 2
        var defer_tast_result178 int = x179 + x182
        println__T_string("pattern:cleanup")
        return defer_tast_result178
    default:
        var defer_return184 int = 0
        println__T_string("pattern:cleanup")
        return defer_return184
    }
}

func closure_cleanup() struct{} {
    var run__9 closure_env_run_0 = closure_env_run_0{}
    _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__9)
    println__T_string("closure:after")
    println__T_string("closure:outer")
    return struct{}{}
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t236 int = early_return()
    var t237 string = _goml_m_inherent_i_int_i_int_i_to__string(t236)
    println__T_string(t237)
    maybe(None{})
    loop_cleanup()
    observed_at_exit()
    var t238 Option__int = Some{
        _0: 3,
    }
    var t239 int = pattern_cleanup(t238)
    var t240 string = _goml_m_inherent_i_int_i_int_i_to__string(t239)
    println__T_string(t240)
    var t241 int = pattern_cleanup(None{})
    var t242 string = _goml_m_inherent_i_int_i_int_i_to__string(t241)
    println__T_string(t242)
    closure_cleanup()
    println__T_string("main:second")
    println__T_string("main:first")
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t244)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t248 *ref_int_x = ref__Ref_3int(value__207)
    return t248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t251 int = ref_get__Ref_3int(self__208)
    return t251
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t256 bool = self__59 == other__60
    return t256
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t259 string = _goml_runtime_core_int_to_string(self__5)
    return t259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var t262 *ref_string_x = ref__Ref_6string(value__207)
    return t262
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var t267 string = ref_get__Ref_6string(self__208)
    return t267
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env204 closure_env_run_0) struct{} {
    println__T_string("closure:body")
    println__T_string("closure:inner")
    return struct{}{}
}

func main() {
    main0()
}
