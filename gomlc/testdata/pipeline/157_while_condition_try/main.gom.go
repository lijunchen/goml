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
    var inline286 int32 = 0
    var inline287 *ref_int32_x = ref__Ref_5int32(inline286)
    i__2 = inline287
    var total__3 *ref_int32_x
    var inline283 int32 = 0
    var inline284 *ref_int32_x = ref__Ref_5int32(inline283)
    total__3 = inline284
    Loop_loop208:
    for {
        var t209 int32
        var inline279 int32 = ref_get__Ref_5int32(i__2)
        t209 = inline279
        var mtmp177 Option__bool
        var inline275 bool = t209 < 3
        if inline275 {
            var inline276 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp177 = inline276
        } else {
            var inline277 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp177 = inline277
        }
        var jp211 bool
        switch mtmp177.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x178 bool = mtmp177.(Option__bool_Some)._0
            jp211 = x178
            if jp211 {
                var t212 int32
                var inline273 int32 = ref_get__Ref_5int32(total__3)
                t212 = inline273
                var t213 int32
                var inline271 int32 = ref_get__Ref_5int32(i__2)
                t213 = inline271
                var t214 int32 = t212 + t213
                ref_set__Ref_5int32(total__3, t214)
                var t215 int32
                var inline267 int32 = ref_get__Ref_5int32(i__2)
                t215 = inline267
                var t216 int32 = t215 + 1
                ref_set__Ref_5int32(i__2, t216)
                continue
            } else {
                break Loop_loop208
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t206 int32
    var inline281 int32 = ref_get__Ref_5int32(total__3)
    t206 = inline281
    var t207 Option__int32 = Option__int32_Some{
        _0: t206,
    }
    return t207
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline309 int32 = 0
    var inline310 *ref_int32_x = ref__Ref_5int32(inline309)
    i__4 = inline310
    var total__5 *ref_int32_x
    var inline306 int32 = 0
    var inline307 *ref_int32_x = ref__Ref_5int32(inline306)
    total__5 = inline307
    Loop_loop222:
    for {
        var t223 int32
        var inline302 int32 = ref_get__Ref_5int32(i__4)
        t223 = inline302
        var mtmp182 Option__bool
        var inline299 bool = t223 < 2
        if inline299 {
            var inline300 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp182 = inline300
        } else {
            mtmp182 = Option__bool_None{}
        }
        var jp225 bool
        switch mtmp182.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x183 bool = mtmp182.(Option__bool_Some)._0
            jp225 = x183
            if jp225 {
                var t226 int32
                var inline297 int32 = ref_get__Ref_5int32(total__5)
                t226 = inline297
                var t227 int32
                var inline295 int32 = ref_get__Ref_5int32(i__4)
                t227 = inline295
                var t228 int32 = t226 + t227
                ref_set__Ref_5int32(total__5, t228)
                var t229 int32
                var inline291 int32 = ref_get__Ref_5int32(i__4)
                t229 = inline291
                var t230 int32 = t229 + 1
                ref_set__Ref_5int32(i__4, t230)
                continue
            } else {
                break Loop_loop222
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t220 int32
    var inline304 int32 = ref_get__Ref_5int32(total__5)
    t220 = inline304
    var t221 Option__int32 = Option__int32_Some{
        _0: t220,
    }
    return t221
}

func main0() struct{} {
    var t238 Option__int32 = run_some()
    var t239 string
    switch t238.(type) {
    case Option__int32_None:
        t239 = "none"
    case Option__int32_Some:
        var inline325 int32 = t238.(Option__int32_Some)._0
        var inline327 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline325)
        var inline328 string = "some=" + inline327
        t239 = inline328
    default:
        panic("non-exhaustive match")
    }
    var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline322)
    var t240 Option__int32 = run_none()
    var t241 string
    switch t240.(type) {
    case Option__int32_None:
        t241 = "none"
    case Option__int32_Some:
        var inline317 int32 = t240.(Option__int32_Some)._0
        var inline319 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline317)
        var inline320 string = "some=" + inline319
        t241 = inline320
    default:
        panic("non-exhaustive match")
    }
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline314)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t252 string = _goml_runtime_core_int32_to_string(self__35)
    return t252
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
