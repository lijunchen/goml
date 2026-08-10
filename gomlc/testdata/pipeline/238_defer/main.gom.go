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
    var defer_return172 int = 7
    var inline303 string = "return:inner"
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline303)
    _goml_runtime_core_string_println(inline304)
    var inline299 string = "return:outer"
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline299)
    _goml_runtime_core_string_println(inline300)
    return defer_return172
}

func maybe(value__0 Option__int) Option__int {
    var jp227 int
    switch value__0.(type) {
    case None:
        var defer_return181 Option__int = None{}
        var inline307 string = "try:cleanup"
        var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline307)
        _goml_runtime_core_string_println(inline308)
        return defer_return181
    case Some:
        var x180 int = value__0.(Some)._0
        jp227 = x180
        var defer_result183 Option__int = Some{
            _0: jp227,
        }
        var inline311 string = "try:cleanup"
        var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline311)
        _goml_runtime_core_string_println(inline312)
        return defer_result183
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline336 int = 0
    var inline337 *ref_int_x = ref__Ref_3int(inline336)
    index__2 = inline337
    Loop_loop230:
    for {
        var t231 int
        var inline334 int = ref_get__Ref_3int(index__2)
        t231 = inline334
        var t232 bool = t231 < 3
        if t232 {
            var current__3 int
            var inline332 int = ref_get__Ref_3int(index__2)
            current__3 = inline332
            var t233 int = current__3 + 1
            ref_set__Ref_3int(index__2, t233)
            var t237 bool = current__3 == 0
            if t237 {
                var t238 string
                var inline318 string = _goml_runtime_core_int_to_string(current__3)
                t238 = inline318
                var t239 string = "loop:" + t238
                var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
                _goml_runtime_core_string_println(inline315)
                continue
            } else {
                var t241 bool = current__3 == 1
                if t241 {
                    var t242 string
                    var inline323 string = _goml_runtime_core_int_to_string(current__3)
                    t242 = inline323
                    var t243 string = "loop:" + t242
                    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
                    _goml_runtime_core_string_println(inline320)
                    break Loop_loop230
                } else {
                    var t235 string
                    var inline328 string = _goml_runtime_core_int_to_string(current__3)
                    t235 = inline328
                    var t236 string = "loop:" + t235
                    var inline325 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
                    _goml_runtime_core_string_println(inline325)
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
        var inline350 string = "pattern:cleanup"
        var inline351 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline350)
        _goml_runtime_core_string_println(inline351)
        return defer_tast_result195
    default:
        var defer_return201 int = 0
        var inline354 string = "pattern:cleanup"
        var inline355 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline354)
        _goml_runtime_core_string_println(inline355)
        return defer_return201
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t254 int = early_return()
    var t255 string
    var inline412 string = _goml_runtime_core_int_to_string(t254)
    t255 = inline412
    var inline409 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t255)
    _goml_runtime_core_string_println(inline409)
    maybe(None{})
    loop_cleanup()
    var inline403 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline403, "after")
    var inline405 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline403)
    var inline406 string = "observed:" + inline405
    println__T_string(inline406)
    var t256 Option__int = Some{
        _0: 3,
    }
    var t257 int = pattern_cleanup(t256)
    var t258 string
    var inline401 string = _goml_runtime_core_int_to_string(t257)
    t258 = inline401
    var inline398 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
    _goml_runtime_core_string_println(inline398)
    var t259 int
    var inline395 int = 0
    println__T_string("pattern:cleanup")
    t259 = inline395
    var t260 string
    var inline383 string = _goml_runtime_core_int_to_string(t259)
    t260 = inline383
    var inline380 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline380)
    var inline374 closure_env_run_0 = closure_env_run_0{}
    var inline375 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline374)
    }
    inline375()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline370 string = "main:second"
    var inline371 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline370)
    _goml_runtime_core_string_println(inline371)
    var inline366 string = "main:first"
    var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline366)
    _goml_runtime_core_string_println(inline367)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t262 string
    t262 = value__1
    _goml_runtime_core_string_println(t262)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__255 string) *ref_string_x {
    var t277 *ref_string_x = ref__Ref_6string(value__255)
    return t277
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__257 *ref_string_x, value__258 string) struct{} {
    ref_set__Ref_6string(self__257, value__258)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__256 *ref_string_x) string {
    var t282 string = ref_get__Ref_6string(self__256)
    return t282
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env221 closure_env_run_0) struct{} {
    var inline419 string = "closure:body"
    var inline420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline419)
    _goml_runtime_core_string_println(inline420)
    var inline415 string = "closure:inner"
    var inline416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline415)
    _goml_runtime_core_string_println(inline416)
    return struct{}{}
}

func main() {
    main0()
}
