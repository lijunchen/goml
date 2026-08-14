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

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func run_some() Option__int32 {
    var i__2 *ref_int32_x
    var inline296 int32 = 0
    var inline297 *ref_int32_x = ref__Ref_5int32(inline296)
    i__2 = inline297
    var total__3 *ref_int32_x
    var inline293 int32 = 0
    var inline294 *ref_int32_x = ref__Ref_5int32(inline293)
    total__3 = inline294
    Loop_loop218:
    for {
        var t219 int32
        var inline289 int32 = ref_get__Ref_5int32(i__2)
        t219 = inline289
        var mtmp187 Option__bool
        var inline285 bool = t219 < 3
        if inline285 {
            var inline286 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp187 = inline286
        } else {
            var inline287 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp187 = inline287
        }
        var jp221 bool
        switch mtmp187.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x188 bool = mtmp187.(Option__bool_Some)._0
            jp221 = x188
            if jp221 {
                var t222 int32
                var inline283 int32 = ref_get__Ref_5int32(total__3)
                t222 = inline283
                var t223 int32
                var inline281 int32 = ref_get__Ref_5int32(i__2)
                t223 = inline281
                var t224 int32 = t222 + t223
                ref_set__Ref_5int32(total__3, t224)
                var t225 int32
                var inline277 int32 = ref_get__Ref_5int32(i__2)
                t225 = inline277
                var t226 int32 = t225 + 1
                ref_set__Ref_5int32(i__2, t226)
                continue
            } else {
                break Loop_loop218
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t216 int32
    var inline291 int32 = ref_get__Ref_5int32(total__3)
    t216 = inline291
    var t217 Option__int32 = Option__int32_Some{
        _0: t216,
    }
    return t217
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline319 int32 = 0
    var inline320 *ref_int32_x = ref__Ref_5int32(inline319)
    i__4 = inline320
    var total__5 *ref_int32_x
    var inline316 int32 = 0
    var inline317 *ref_int32_x = ref__Ref_5int32(inline316)
    total__5 = inline317
    Loop_loop232:
    for {
        var t233 int32
        var inline312 int32 = ref_get__Ref_5int32(i__4)
        t233 = inline312
        var mtmp192 Option__bool
        var inline309 bool = t233 < 2
        if inline309 {
            var inline310 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp192 = inline310
        } else {
            mtmp192 = Option__bool_None{}
        }
        var jp235 bool
        switch mtmp192.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x193 bool = mtmp192.(Option__bool_Some)._0
            jp235 = x193
            if jp235 {
                var t236 int32
                var inline307 int32 = ref_get__Ref_5int32(total__5)
                t236 = inline307
                var t237 int32
                var inline305 int32 = ref_get__Ref_5int32(i__4)
                t237 = inline305
                var t238 int32 = t236 + t237
                ref_set__Ref_5int32(total__5, t238)
                var t239 int32
                var inline301 int32 = ref_get__Ref_5int32(i__4)
                t239 = inline301
                var t240 int32 = t239 + 1
                ref_set__Ref_5int32(i__4, t240)
                continue
            } else {
                break Loop_loop232
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t230 int32
    var inline314 int32 = ref_get__Ref_5int32(total__5)
    t230 = inline314
    var t231 Option__int32 = Option__int32_Some{
        _0: t230,
    }
    return t231
}

func main0() struct{} {
    var t248 Option__int32 = run_some()
    var t249 string
    switch t248.(type) {
    case Option__int32_None:
        t249 = "none"
    case Option__int32_Some:
        var inline335 int32 = t248.(Option__int32_Some)._0
        var inline337 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline335)
        var inline338 string = "some=" + inline337
        t249 = inline338
    default:
        panic("non-exhaustive match")
    }
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline332)
    var t250 Option__int32 = run_none()
    var t251 string
    switch t250.(type) {
    case Option__int32_None:
        t251 = "none"
    case Option__int32_Some:
        var inline327 int32 = t250.(Option__int32_Some)._0
        var inline329 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline327)
        var inline330 string = "some=" + inline329
        t251 = inline330
    default:
        panic("non-exhaustive match")
    }
    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline324)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t262 string = _goml_runtime_core_int32_to_string(self__33)
    return t262
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
