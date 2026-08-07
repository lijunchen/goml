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
    var defer_return136 int = 7
    var inline266 string = "return:inner"
    var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline266)
    _goml_runtime_core_string_println(inline267)
    var inline262 string = "return:outer"
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline262)
    _goml_runtime_core_string_println(inline263)
    return defer_return136
}

func maybe(value__0 Option__int) Option__int {
    var jp191 int
    switch value__0.(type) {
    case None:
        var defer_return145 Option__int = None{}
        var inline270 string = "try:cleanup"
        var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline270)
        _goml_runtime_core_string_println(inline271)
        return defer_return145
    case Some:
        var x144 int = value__0.(Some)._0
        jp191 = x144
        var defer_result147 Option__int = Some{
            _0: jp191,
        }
        var inline274 string = "try:cleanup"
        var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline274)
        _goml_runtime_core_string_println(inline275)
        return defer_result147
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline305 int = 0
    var inline306 *ref_int_x = ref__Ref_3int(inline305)
    index__2 = inline306
    Loop_loop194:
    for {
        var t195 int
        var inline303 int = ref_get__Ref_3int(index__2)
        t195 = inline303
        var t196 bool = t195 < 3
        if t196 {
            var current__3 int
            var inline301 int = ref_get__Ref_3int(index__2)
            current__3 = inline301
            var t197 int = current__3 + 1
            ref_set__Ref_3int(index__2, t197)
            var t201 bool
            var inline296 int = 0
            var inline297 bool = current__3 == inline296
            t201 = inline297
            if t201 {
                var t202 string
                var inline281 string = _goml_runtime_core_int_to_string(current__3)
                t202 = inline281
                var t203 string = "loop:" + t202
                var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
                _goml_runtime_core_string_println(inline278)
                continue
            } else {
                var t205 bool
                var inline288 int = 1
                var inline289 bool = current__3 == inline288
                t205 = inline289
                if t205 {
                    var t206 string
                    var inline286 string = _goml_runtime_core_int_to_string(current__3)
                    t206 = inline286
                    var t207 string = "loop:" + t206
                    var inline283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                    _goml_runtime_core_string_println(inline283)
                    break Loop_loop194
                } else {
                    var t199 string
                    var inline294 string = _goml_runtime_core_int_to_string(current__3)
                    t199 = inline294
                    var t200 string = "loop:" + t199
                    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
                    _goml_runtime_core_string_println(inline291)
                    continue
                }
            }
        } else {
            break Loop_loop194
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x160 int = value__5.(Some)._0
        var x163 int = 2
        var defer_tast_result159 int = x160 + x163
        var inline319 string = "pattern:cleanup"
        var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline319)
        _goml_runtime_core_string_println(inline320)
        return defer_tast_result159
    default:
        var defer_return165 int = 0
        var inline323 string = "pattern:cleanup"
        var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline323)
        _goml_runtime_core_string_println(inline324)
        return defer_return165
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t217 int = early_return()
    var t218 string
    var inline383 string = _goml_runtime_core_int_to_string(t217)
    t218 = inline383
    var inline380 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline380)
    maybe(None{})
    loop_cleanup()
    var inline374 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline374, "after")
    var inline376 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline374)
    var inline377 string = "observed:" + inline376
    println__T_string(inline377)
    var t219 Option__int = Some{
        _0: 3,
    }
    var t220 int = pattern_cleanup(t219)
    var t221 string
    var inline372 string = _goml_runtime_core_int_to_string(t220)
    t221 = inline372
    var inline369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline369)
    var t222 int
    var inline366 int = 0
    println__T_string("pattern:cleanup")
    t222 = inline366
    var t223 string
    var inline354 string = _goml_runtime_core_int_to_string(t222)
    t223 = inline354
    var inline351 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline351)
    var inline346 closure_env_run_0 = closure_env_run_0{}
    _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline346)
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline342 string = "main:second"
    var inline343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline342)
    _goml_runtime_core_string_println(inline343)
    var inline338 string = "main:first"
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline338)
    _goml_runtime_core_string_println(inline339)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t225 string
    t225 = value__31
    _goml_runtime_core_string_println(t225)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__232 string) *ref_string_x {
    var t243 *ref_string_x = ref__Ref_6string(value__232)
    return t243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__234 *ref_string_x, value__235 string) struct{} {
    ref_set__Ref_6string(self__234, value__235)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__233 *ref_string_x) string {
    var t248 string = ref_get__Ref_6string(self__233)
    return t248
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env185 closure_env_run_0) struct{} {
    var inline390 string = "closure:body"
    var inline391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline390)
    _goml_runtime_core_string_println(inline391)
    var inline386 string = "closure:inner"
    var inline387 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline386)
    _goml_runtime_core_string_println(inline387)
    return struct{}{}
}

func main() {
    main0()
}
