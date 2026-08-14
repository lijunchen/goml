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
    var inline291 int32 = 0
    var inline292 *ref_int32_x = ref__Ref_5int32(inline291)
    i__2 = inline292
    var total__3 *ref_int32_x
    var inline288 int32 = 0
    var inline289 *ref_int32_x = ref__Ref_5int32(inline288)
    total__3 = inline289
    Loop_loop213:
    for {
        var t214 int32
        var inline284 int32 = ref_get__Ref_5int32(i__2)
        t214 = inline284
        var mtmp182 Option__bool
        var inline280 bool = t214 < 3
        if inline280 {
            var inline281 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp182 = inline281
        } else {
            var inline282 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp182 = inline282
        }
        var jp216 bool
        switch mtmp182.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x183 bool = mtmp182.(Option__bool_Some)._0
            jp216 = x183
            if jp216 {
                var t217 int32
                var inline278 int32 = ref_get__Ref_5int32(total__3)
                t217 = inline278
                var t218 int32
                var inline276 int32 = ref_get__Ref_5int32(i__2)
                t218 = inline276
                var t219 int32 = t217 + t218
                ref_set__Ref_5int32(total__3, t219)
                var t220 int32
                var inline272 int32 = ref_get__Ref_5int32(i__2)
                t220 = inline272
                var t221 int32 = t220 + 1
                ref_set__Ref_5int32(i__2, t221)
                continue
            } else {
                break Loop_loop213
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t211 int32
    var inline286 int32 = ref_get__Ref_5int32(total__3)
    t211 = inline286
    var t212 Option__int32 = Option__int32_Some{
        _0: t211,
    }
    return t212
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline314 int32 = 0
    var inline315 *ref_int32_x = ref__Ref_5int32(inline314)
    i__4 = inline315
    var total__5 *ref_int32_x
    var inline311 int32 = 0
    var inline312 *ref_int32_x = ref__Ref_5int32(inline311)
    total__5 = inline312
    Loop_loop227:
    for {
        var t228 int32
        var inline307 int32 = ref_get__Ref_5int32(i__4)
        t228 = inline307
        var mtmp187 Option__bool
        var inline304 bool = t228 < 2
        if inline304 {
            var inline305 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp187 = inline305
        } else {
            mtmp187 = Option__bool_None{}
        }
        var jp230 bool
        switch mtmp187.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x188 bool = mtmp187.(Option__bool_Some)._0
            jp230 = x188
            if jp230 {
                var t231 int32
                var inline302 int32 = ref_get__Ref_5int32(total__5)
                t231 = inline302
                var t232 int32
                var inline300 int32 = ref_get__Ref_5int32(i__4)
                t232 = inline300
                var t233 int32 = t231 + t232
                ref_set__Ref_5int32(total__5, t233)
                var t234 int32
                var inline296 int32 = ref_get__Ref_5int32(i__4)
                t234 = inline296
                var t235 int32 = t234 + 1
                ref_set__Ref_5int32(i__4, t235)
                continue
            } else {
                break Loop_loop227
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t225 int32
    var inline309 int32 = ref_get__Ref_5int32(total__5)
    t225 = inline309
    var t226 Option__int32 = Option__int32_Some{
        _0: t225,
    }
    return t226
}

func main0() struct{} {
    var t243 Option__int32 = run_some()
    var t244 string
    switch t243.(type) {
    case Option__int32_None:
        t244 = "none"
    case Option__int32_Some:
        var inline330 int32 = t243.(Option__int32_Some)._0
        var inline332 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline330)
        var inline333 string = "some=" + inline332
        t244 = inline333
    default:
        panic("non-exhaustive match")
    }
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline327)
    var t245 Option__int32 = run_none()
    var t246 string
    switch t245.(type) {
    case Option__int32_None:
        t246 = "none"
    case Option__int32_Some:
        var inline322 int32 = t245.(Option__int32_Some)._0
        var inline324 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline322)
        var inline325 string = "some=" + inline324
        t246 = inline325
    default:
        panic("non-exhaustive match")
    }
    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline319)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t257 string = _goml_runtime_core_int32_to_string(self__33)
    return t257
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
