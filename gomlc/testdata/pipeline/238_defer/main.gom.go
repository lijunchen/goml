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
    var defer_return177 int = 7
    var inline307 string = "return:inner"
    var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline307)
    _goml_runtime_core_string_println(inline308)
    var inline303 string = "return:outer"
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline303)
    _goml_runtime_core_string_println(inline304)
    return defer_return177
}

func maybe(value__0 Option__int) Option__int {
    var jp232 int
    switch value__0.(type) {
    case None:
        var defer_return186 Option__int = None{}
        var inline311 string = "try:cleanup"
        var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline311)
        _goml_runtime_core_string_println(inline312)
        return defer_return186
    case Some:
        var x185 int = value__0.(Some)._0
        jp232 = x185
        var defer_result188 Option__int = Some{
            _0: jp232,
        }
        var inline315 string = "try:cleanup"
        var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline315)
        _goml_runtime_core_string_println(inline316)
        return defer_result188
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline346 int = 0
    var inline347 *ref_int_x = ref__Ref_3int(inline346)
    index__2 = inline347
    Loop_loop235:
    for {
        var t236 int
        var inline344 int = ref_get__Ref_3int(index__2)
        t236 = inline344
        var t237 bool = t236 < 3
        if t237 {
            var current__3 int
            var inline342 int = ref_get__Ref_3int(index__2)
            current__3 = inline342
            var t238 int = current__3 + 1
            ref_set__Ref_3int(index__2, t238)
            var t242 bool
            var inline337 int = 0
            var inline338 bool = current__3 == inline337
            t242 = inline338
            if t242 {
                var t243 string
                var inline322 string = _goml_runtime_core_int_to_string(current__3)
                t243 = inline322
                var t244 string = "loop:" + t243
                var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
                _goml_runtime_core_string_println(inline319)
                continue
            } else {
                var t246 bool
                var inline329 int = 1
                var inline330 bool = current__3 == inline329
                t246 = inline330
                if t246 {
                    var t247 string
                    var inline327 string = _goml_runtime_core_int_to_string(current__3)
                    t247 = inline327
                    var t248 string = "loop:" + t247
                    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t248)
                    _goml_runtime_core_string_println(inline324)
                    break Loop_loop235
                } else {
                    var t240 string
                    var inline335 string = _goml_runtime_core_int_to_string(current__3)
                    t240 = inline335
                    var t241 string = "loop:" + t240
                    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
                    _goml_runtime_core_string_println(inline332)
                    continue
                }
            }
        } else {
            break Loop_loop235
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x201 int = value__5.(Some)._0
        var x204 int = 2
        var defer_tast_result200 int = x201 + x204
        var inline360 string = "pattern:cleanup"
        var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline360)
        _goml_runtime_core_string_println(inline361)
        return defer_tast_result200
    default:
        var defer_return206 int = 0
        var inline364 string = "pattern:cleanup"
        var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline364)
        _goml_runtime_core_string_println(inline365)
        return defer_return206
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t258 int = early_return()
    var t259 string
    var inline424 string = _goml_runtime_core_int_to_string(t258)
    t259 = inline424
    var inline421 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t259)
    _goml_runtime_core_string_println(inline421)
    maybe(None{})
    loop_cleanup()
    var inline415 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline415, "after")
    var inline417 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline415)
    var inline418 string = "observed:" + inline417
    println__T_string(inline418)
    var t260 Option__int = Some{
        _0: 3,
    }
    var t261 int = pattern_cleanup(t260)
    var t262 string
    var inline413 string = _goml_runtime_core_int_to_string(t261)
    t262 = inline413
    var inline410 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline410)
    var t263 int
    var inline407 int = 0
    println__T_string("pattern:cleanup")
    t263 = inline407
    var t264 string
    var inline395 string = _goml_runtime_core_int_to_string(t263)
    t264 = inline395
    var inline392 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t264)
    _goml_runtime_core_string_println(inline392)
    var inline387 closure_env_run_0 = closure_env_run_0{}
    _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline387)
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline383 string = "main:second"
    var inline384 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline383)
    _goml_runtime_core_string_println(inline384)
    var inline379 string = "main:first"
    var inline380 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline379)
    _goml_runtime_core_string_println(inline380)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t266 string
    t266 = value__31
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__236 string) *ref_string_x {
    var t284 *ref_string_x = ref__Ref_6string(value__236)
    return t284
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__238 *ref_string_x, value__239 string) struct{} {
    ref_set__Ref_6string(self__238, value__239)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__237 *ref_string_x) string {
    var t289 string = ref_get__Ref_6string(self__237)
    return t289
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env226 closure_env_run_0) struct{} {
    var inline431 string = "closure:body"
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline431)
    _goml_runtime_core_string_println(inline432)
    var inline427 string = "closure:inner"
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline427)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func main() {
    main0()
}
