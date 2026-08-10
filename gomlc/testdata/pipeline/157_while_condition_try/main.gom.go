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
    var inline281 int32 = 0
    var inline282 *ref_int32_x = ref__Ref_5int32(inline281)
    i__2 = inline282
    var total__3 *ref_int32_x
    var inline278 int32 = 0
    var inline279 *ref_int32_x = ref__Ref_5int32(inline278)
    total__3 = inline279
    Loop_loop203:
    for {
        var t204 int32
        var inline274 int32 = ref_get__Ref_5int32(i__2)
        t204 = inline274
        var mtmp172 Option__bool
        var inline270 bool = t204 < 3
        if inline270 {
            var inline271 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp172 = inline271
        } else {
            var inline272 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp172 = inline272
        }
        var jp206 bool
        switch mtmp172.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x173 bool = mtmp172.(Option__bool_Some)._0
            jp206 = x173
            if jp206 {
                var t207 int32
                var inline268 int32 = ref_get__Ref_5int32(total__3)
                t207 = inline268
                var t208 int32
                var inline266 int32 = ref_get__Ref_5int32(i__2)
                t208 = inline266
                var t209 int32 = t207 + t208
                ref_set__Ref_5int32(total__3, t209)
                var t210 int32
                var inline262 int32 = ref_get__Ref_5int32(i__2)
                t210 = inline262
                var t211 int32 = t210 + 1
                ref_set__Ref_5int32(i__2, t211)
                continue
            } else {
                break Loop_loop203
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t201 int32
    var inline276 int32 = ref_get__Ref_5int32(total__3)
    t201 = inline276
    var t202 Option__int32 = Option__int32_Some{
        _0: t201,
    }
    return t202
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline304 int32 = 0
    var inline305 *ref_int32_x = ref__Ref_5int32(inline304)
    i__4 = inline305
    var total__5 *ref_int32_x
    var inline301 int32 = 0
    var inline302 *ref_int32_x = ref__Ref_5int32(inline301)
    total__5 = inline302
    Loop_loop217:
    for {
        var t218 int32
        var inline297 int32 = ref_get__Ref_5int32(i__4)
        t218 = inline297
        var mtmp177 Option__bool
        var inline294 bool = t218 < 2
        if inline294 {
            var inline295 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp177 = inline295
        } else {
            mtmp177 = Option__bool_None{}
        }
        var jp220 bool
        switch mtmp177.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x178 bool = mtmp177.(Option__bool_Some)._0
            jp220 = x178
            if jp220 {
                var t221 int32
                var inline292 int32 = ref_get__Ref_5int32(total__5)
                t221 = inline292
                var t222 int32
                var inline290 int32 = ref_get__Ref_5int32(i__4)
                t222 = inline290
                var t223 int32 = t221 + t222
                ref_set__Ref_5int32(total__5, t223)
                var t224 int32
                var inline286 int32 = ref_get__Ref_5int32(i__4)
                t224 = inline286
                var t225 int32 = t224 + 1
                ref_set__Ref_5int32(i__4, t225)
                continue
            } else {
                break Loop_loop217
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t215 int32
    var inline299 int32 = ref_get__Ref_5int32(total__5)
    t215 = inline299
    var t216 Option__int32 = Option__int32_Some{
        _0: t215,
    }
    return t216
}

func main0() struct{} {
    var t233 Option__int32 = run_some()
    var t234 string
    switch t233.(type) {
    case Option__int32_None:
        t234 = "none"
    case Option__int32_Some:
        var inline320 int32 = t233.(Option__int32_Some)._0
        var inline322 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline320)
        var inline323 string = "some=" + inline322
        t234 = inline323
    default:
        panic("non-exhaustive match")
    }
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline317)
    var t235 Option__int32 = run_none()
    var t236 string
    switch t235.(type) {
    case Option__int32_None:
        t236 = "none"
    case Option__int32_Some:
        var inline312 int32 = t235.(Option__int32_Some)._0
        var inline314 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline312)
        var inline315 string = "some=" + inline314
        t236 = inline315
    default:
        panic("non-exhaustive match")
    }
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline309)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t247 string = _goml_runtime_core_int32_to_string(self__33)
    return t247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
