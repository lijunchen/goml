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
    var defer_return172 int = 7
    var inline302 string = "return:inner"
    var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline302)
    _goml_runtime_core_string_println(inline303)
    var inline298 string = "return:outer"
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline298)
    _goml_runtime_core_string_println(inline299)
    return defer_return172
}

func maybe(value__0 Option__int) Option__int {
    var jp227 int
    switch value__0.(type) {
    case None:
        var defer_return181 Option__int = None{}
        var inline306 string = "try:cleanup"
        var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline306)
        _goml_runtime_core_string_println(inline307)
        return defer_return181
    case Some:
        var x180 int = value__0.(Some)._0
        jp227 = x180
        var defer_result183 Option__int = Some{
            _0: jp227,
        }
        var inline310 string = "try:cleanup"
        var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline310)
        _goml_runtime_core_string_println(inline311)
        return defer_result183
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline341 int = 0
    var inline342 *ref_int_x = ref__Ref_3int(inline341)
    index__2 = inline342
    Loop_loop230:
    for {
        var t231 int
        var inline339 int = ref_get__Ref_3int(index__2)
        t231 = inline339
        var t232 bool = t231 < 3
        if t232 {
            var current__3 int
            var inline337 int = ref_get__Ref_3int(index__2)
            current__3 = inline337
            var t233 int = current__3 + 1
            ref_set__Ref_3int(index__2, t233)
            var t237 bool
            var inline332 int = 0
            var inline333 bool = current__3 == inline332
            t237 = inline333
            if t237 {
                var t238 string
                var inline317 string = _goml_runtime_core_int_to_string(current__3)
                t238 = inline317
                var t239 string = "loop:" + t238
                var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
                _goml_runtime_core_string_println(inline314)
                continue
            } else {
                var t241 bool
                var inline324 int = 1
                var inline325 bool = current__3 == inline324
                t241 = inline325
                if t241 {
                    var t242 string
                    var inline322 string = _goml_runtime_core_int_to_string(current__3)
                    t242 = inline322
                    var t243 string = "loop:" + t242
                    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
                    _goml_runtime_core_string_println(inline319)
                    break Loop_loop230
                } else {
                    var t235 string
                    var inline330 string = _goml_runtime_core_int_to_string(current__3)
                    t235 = inline330
                    var t236 string = "loop:" + t235
                    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
                    _goml_runtime_core_string_println(inline327)
                    continue
                }
            }
        } else {
            break Loop_loop230
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x196 int = value__5.(Some)._0
        var x199 int = 2
        var defer_tast_result195 int = x196 + x199
        var inline355 string = "pattern:cleanup"
        var inline356 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline355)
        _goml_runtime_core_string_println(inline356)
        return defer_tast_result195
    default:
        var defer_return201 int = 0
        var inline359 string = "pattern:cleanup"
        var inline360 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline359)
        _goml_runtime_core_string_println(inline360)
        return defer_return201
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t253 int = early_return()
    var t254 string
    var inline419 string = _goml_runtime_core_int_to_string(t253)
    t254 = inline419
    var inline416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline416)
    maybe(None{})
    loop_cleanup()
    var inline410 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline410, "after")
    var inline412 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline410)
    var inline413 string = "observed:" + inline412
    println__T_string(inline413)
    var t255 Option__int = Some{
        _0: 3,
    }
    var t256 int = pattern_cleanup(t255)
    var t257 string
    var inline408 string = _goml_runtime_core_int_to_string(t256)
    t257 = inline408
    var inline405 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline405)
    var t258 int
    var inline402 int = 0
    println__T_string("pattern:cleanup")
    t258 = inline402
    var t259 string
    var inline390 string = _goml_runtime_core_int_to_string(t258)
    t259 = inline390
    var inline387 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t259)
    _goml_runtime_core_string_println(inline387)
    var inline382 closure_env_run_0 = closure_env_run_0{}
    _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline382)
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline378 string = "main:second"
    var inline379 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline378)
    _goml_runtime_core_string_println(inline379)
    var inline374 string = "main:first"
    var inline375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline374)
    _goml_runtime_core_string_println(inline375)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t261 string
    t261 = value__31
    _goml_runtime_core_string_println(t261)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__257 string) *ref_string_x {
    var t279 *ref_string_x = ref__Ref_6string(value__257)
    return t279
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__259 *ref_string_x, value__260 string) struct{} {
    ref_set__Ref_6string(self__259, value__260)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__258 *ref_string_x) string {
    var t284 string = ref_get__Ref_6string(self__258)
    return t284
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env221 closure_env_run_0) struct{} {
    var inline426 string = "closure:body"
    var inline427 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline426)
    _goml_runtime_core_string_println(inline427)
    var inline422 string = "closure:inner"
    var inline423 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline422)
    _goml_runtime_core_string_println(inline423)
    return struct{}{}
}

func main() {
    main0()
}
