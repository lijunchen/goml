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
    var defer_return182 int = 7
    var inline313 string = "return:inner"
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline313)
    _goml_runtime_core_string_println(inline314)
    var inline309 string = "return:outer"
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline309)
    _goml_runtime_core_string_println(inline310)
    return defer_return182
}

func maybe(value__0 Option__int) Option__int {
    var jp237 int
    switch value__0.(type) {
    case None:
        var defer_return191 Option__int = None{}
        var inline317 string = "try:cleanup"
        var inline318 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline317)
        _goml_runtime_core_string_println(inline318)
        return defer_return191
    case Some:
        var x190 int = value__0.(Some)._0
        jp237 = x190
        var defer_result193 Option__int = Some{
            _0: jp237,
        }
        var inline321 string = "try:cleanup"
        var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline321)
        _goml_runtime_core_string_println(inline322)
        return defer_result193
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline346 int = 0
    var inline347 *ref_int_x = ref__Ref_3int(inline346)
    index__2 = inline347
    Loop_loop240:
    for {
        var t241 int
        var inline344 int = ref_get__Ref_3int(index__2)
        t241 = inline344
        var t242 bool = t241 < 3
        if t242 {
            var current__3 int
            var inline342 int = ref_get__Ref_3int(index__2)
            current__3 = inline342
            var t243 int = current__3 + 1
            ref_set__Ref_3int(index__2, t243)
            var t247 bool = current__3 == 0
            if t247 {
                var t248 string
                var inline328 string = _goml_runtime_core_int_to_string(current__3)
                t248 = inline328
                var t249 string = "loop:" + t248
                var inline325 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
                _goml_runtime_core_string_println(inline325)
                continue
            } else {
                var t251 bool = current__3 == 1
                if t251 {
                    var t252 string
                    var inline333 string = _goml_runtime_core_int_to_string(current__3)
                    t252 = inline333
                    var t253 string = "loop:" + t252
                    var inline330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
                    _goml_runtime_core_string_println(inline330)
                    break Loop_loop240
                } else {
                    var t245 string
                    var inline338 string = _goml_runtime_core_int_to_string(current__3)
                    t245 = inline338
                    var t246 string = "loop:" + t245
                    var inline335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
                    _goml_runtime_core_string_println(inline335)
                    continue
                }
            }
        } else {
            break Loop_loop240
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x206 int = value__5.(Some)._0
        var x209 int = 2
        var defer_tast_result205 int = x206 + x209
        var inline360 string = "pattern:cleanup"
        var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline360)
        _goml_runtime_core_string_println(inline361)
        return defer_tast_result205
    default:
        var defer_return211 int = 0
        var inline364 string = "pattern:cleanup"
        var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline364)
        _goml_runtime_core_string_println(inline365)
        return defer_return211
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t264 int = early_return()
    var t265 string
    var inline422 string = _goml_runtime_core_int_to_string(t264)
    t265 = inline422
    var inline419 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline419)
    maybe(None{})
    loop_cleanup()
    var inline413 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline413, "after")
    var inline415 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline413)
    var inline416 string = "observed:" + inline415
    println__T_string(inline416)
    var t266 Option__int = Some{
        _0: 3,
    }
    var t267 int = pattern_cleanup(t266)
    var t268 string
    var inline411 string = _goml_runtime_core_int_to_string(t267)
    t268 = inline411
    var inline408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t268)
    _goml_runtime_core_string_println(inline408)
    var t269 int
    var inline405 int = 0
    println__T_string("pattern:cleanup")
    t269 = inline405
    var t270 string
    var inline393 string = _goml_runtime_core_int_to_string(t269)
    t270 = inline393
    var inline390 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline390)
    var inline384 closure_env_run_0 = closure_env_run_0{}
    var inline385 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline384)
    }
    inline385()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline380 string = "main:second"
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline380)
    _goml_runtime_core_string_println(inline381)
    var inline376 string = "main:first"
    var inline377 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline376)
    _goml_runtime_core_string_println(inline377)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t272 string
    t272 = value__1
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__270 string) *ref_string_x {
    var t287 *ref_string_x = ref__Ref_6string(value__270)
    return t287
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__272 *ref_string_x, value__273 string) struct{} {
    ref_set__Ref_6string(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__271 *ref_string_x) string {
    var t292 string = ref_get__Ref_6string(self__271)
    return t292
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env231 closure_env_run_0) struct{} {
    var inline429 string = "closure:body"
    var inline430 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline429)
    _goml_runtime_core_string_println(inline430)
    var inline425 string = "closure:inner"
    var inline426 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline425)
    _goml_runtime_core_string_println(inline426)
    return struct{}{}
}

func main() {
    main0()
}
