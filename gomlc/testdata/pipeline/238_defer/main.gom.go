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
    var inline285 string = "return:inner"
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_println(inline286)
    var inline281 string = "return:outer"
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline281)
    _goml_runtime_core_string_println(inline282)
    return defer_return155
}

func maybe(value__0 Option__int) Option__int {
    var jp210 int
    switch value__0.(type) {
    case None:
        var defer_return164 Option__int = None{}
        var inline289 string = "try:cleanup"
        var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline289)
        _goml_runtime_core_string_println(inline290)
        return defer_return164
    case Some:
        var x163 int = value__0.(Some)._0
        jp210 = x163
        var defer_result166 Option__int = Some{
            _0: jp210,
        }
        var inline293 string = "try:cleanup"
        var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline293)
        _goml_runtime_core_string_println(inline294)
        return defer_result166
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline324 int = 0
    var inline325 *ref_int_x = ref__Ref_3int(inline324)
    index__2 = inline325
    Loop_loop213:
    for {
        var t214 int
        var inline322 int = ref_get__Ref_3int(index__2)
        t214 = inline322
        var t215 bool = t214 < 3
        if t215 {
            var current__3 int
            var inline320 int = ref_get__Ref_3int(index__2)
            current__3 = inline320
            var t216 int = current__3 + 1
            ref_set__Ref_3int(index__2, t216)
            var t220 bool
            var inline315 int = 0
            var inline316 bool = current__3 == inline315
            t220 = inline316
            if t220 {
                var t221 string
                var inline300 string = _goml_runtime_core_int_to_string(current__3)
                t221 = inline300
                var t222 string = "loop:" + t221
                var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
                _goml_runtime_core_string_println(inline297)
                continue
            } else {
                var t224 bool
                var inline307 int = 1
                var inline308 bool = current__3 == inline307
                t224 = inline308
                if t224 {
                    var t225 string
                    var inline305 string = _goml_runtime_core_int_to_string(current__3)
                    t225 = inline305
                    var t226 string = "loop:" + t225
                    var inline302 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
                    _goml_runtime_core_string_println(inline302)
                    break Loop_loop213
                } else {
                    var t218 string
                    var inline313 string = _goml_runtime_core_int_to_string(current__3)
                    t218 = inline313
                    var t219 string = "loop:" + t218
                    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
                    _goml_runtime_core_string_println(inline310)
                    continue
                }
            }
        } else {
            break Loop_loop213
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x179 int = value__5.(Some)._0
        var x182 int = 2
        var defer_tast_result178 int = x179 + x182
        var inline338 string = "pattern:cleanup"
        var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline338)
        _goml_runtime_core_string_println(inline339)
        return defer_tast_result178
    default:
        var defer_return184 int = 0
        var inline342 string = "pattern:cleanup"
        var inline343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline342)
        _goml_runtime_core_string_println(inline343)
        return defer_return184
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t236 int = early_return()
    var t237 string
    var inline402 string = _goml_runtime_core_int_to_string(t236)
    t237 = inline402
    var inline399 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline399)
    maybe(None{})
    loop_cleanup()
    var inline393 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline393, "after")
    var inline395 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline393)
    var inline396 string = "observed:" + inline395
    println__T_string(inline396)
    var t238 Option__int = Some{
        _0: 3,
    }
    var t239 int = pattern_cleanup(t238)
    var t240 string
    var inline391 string = _goml_runtime_core_int_to_string(t239)
    t240 = inline391
    var inline388 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline388)
    var t241 int
    var inline385 int = 0
    println__T_string("pattern:cleanup")
    t241 = inline385
    var t242 string
    var inline373 string = _goml_runtime_core_int_to_string(t241)
    t242 = inline373
    var inline370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline370)
    var inline365 closure_env_run_0 = closure_env_run_0{}
    _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline365)
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline361 string = "main:second"
    var inline362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline361)
    _goml_runtime_core_string_println(inline362)
    var inline357 string = "main:first"
    var inline358 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline357)
    _goml_runtime_core_string_println(inline358)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t244 string
    t244 = value__1
    _goml_runtime_core_string_println(t244)
    return struct{}{}
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
    var inline409 string = "closure:body"
    var inline410 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline409)
    _goml_runtime_core_string_println(inline410)
    var inline405 string = "closure:inner"
    var inline406 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline405)
    _goml_runtime_core_string_println(inline406)
    return struct{}{}
}

func main() {
    main0()
}
