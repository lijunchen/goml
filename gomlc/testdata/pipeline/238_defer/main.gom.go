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
    var defer_return187 int = 7
    var inline318 string = "return:inner"
    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline318)
    _goml_runtime_core_string_println(inline319)
    var inline314 string = "return:outer"
    var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline314)
    _goml_runtime_core_string_println(inline315)
    return defer_return187
}

func maybe(value__0 Option__int) Option__int {
    var jp242 int
    switch value__0.(type) {
    case None:
        var defer_return196 Option__int = None{}
        var inline322 string = "try:cleanup"
        var inline323 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline322)
        _goml_runtime_core_string_println(inline323)
        return defer_return196
    case Some:
        var x195 int = value__0.(Some)._0
        jp242 = x195
        var defer_result198 Option__int = Some{
            _0: jp242,
        }
        var inline326 string = "try:cleanup"
        var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline326)
        _goml_runtime_core_string_println(inline327)
        return defer_result198
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline351 int = 0
    var inline352 *ref_int_x = ref__Ref_3int(inline351)
    index__2 = inline352
    Loop_loop245:
    for {
        var t246 int
        var inline349 int = ref_get__Ref_3int(index__2)
        t246 = inline349
        var t247 bool = t246 < 3
        if t247 {
            var current__3 int
            var inline347 int = ref_get__Ref_3int(index__2)
            current__3 = inline347
            var t248 int = current__3 + 1
            ref_set__Ref_3int(index__2, t248)
            var t252 bool = current__3 == 0
            if t252 {
                var t253 string
                var inline333 string = _goml_runtime_core_int_to_string(current__3)
                t253 = inline333
                var t254 string = "loop:" + t253
                var inline330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
                _goml_runtime_core_string_println(inline330)
                continue
            } else {
                var t256 bool = current__3 == 1
                if t256 {
                    var t257 string
                    var inline338 string = _goml_runtime_core_int_to_string(current__3)
                    t257 = inline338
                    var t258 string = "loop:" + t257
                    var inline335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
                    _goml_runtime_core_string_println(inline335)
                    break Loop_loop245
                } else {
                    var t250 string
                    var inline343 string = _goml_runtime_core_int_to_string(current__3)
                    t250 = inline343
                    var t251 string = "loop:" + t250
                    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
                    _goml_runtime_core_string_println(inline340)
                    continue
                }
            }
        } else {
            break Loop_loop245
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x211 int = value__5.(Some)._0
        var x214 int = 2
        var defer_tast_result210 int = x211 + x214
        var inline365 string = "pattern:cleanup"
        var inline366 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline365)
        _goml_runtime_core_string_println(inline366)
        return defer_tast_result210
    default:
        var defer_return216 int = 0
        var inline369 string = "pattern:cleanup"
        var inline370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline369)
        _goml_runtime_core_string_println(inline370)
        return defer_return216
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t269 int = early_return()
    var t270 string
    var inline427 string = _goml_runtime_core_int_to_string(t269)
    t270 = inline427
    var inline424 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline424)
    maybe(None{})
    loop_cleanup()
    var inline418 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline418, "after")
    var inline420 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline418)
    var inline421 string = "observed:" + inline420
    println__T_string(inline421)
    var t271 Option__int = Some{
        _0: 3,
    }
    var t272 int = pattern_cleanup(t271)
    var t273 string
    var inline416 string = _goml_runtime_core_int_to_string(t272)
    t273 = inline416
    var inline413 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t273)
    _goml_runtime_core_string_println(inline413)
    var t274 int
    var inline410 int = 0
    println__T_string("pattern:cleanup")
    t274 = inline410
    var t275 string
    var inline398 string = _goml_runtime_core_int_to_string(t274)
    t275 = inline398
    var inline395 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t275)
    _goml_runtime_core_string_println(inline395)
    var inline389 closure_env_run_0 = closure_env_run_0{}
    var inline390 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline389)
    }
    inline390()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline385 string = "main:second"
    var inline386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline385)
    _goml_runtime_core_string_println(inline386)
    var inline381 string = "main:first"
    var inline382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline381)
    _goml_runtime_core_string_println(inline382)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t277 string
    t277 = value__1
    _goml_runtime_core_string_println(t277)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__273 string) *ref_string_x {
    var t292 *ref_string_x = ref__Ref_6string(value__273)
    return t292
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__275 *ref_string_x, value__276 string) struct{} {
    ref_set__Ref_6string(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__274 *ref_string_x) string {
    var t297 string = ref_get__Ref_6string(self__274)
    return t297
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env236 closure_env_run_0) struct{} {
    var inline434 string = "closure:body"
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline434)
    _goml_runtime_core_string_println(inline435)
    var inline430 string = "closure:inner"
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline430)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func main() {
    main0()
}
