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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x
    var inline271 int32 = 0
    var inline272 *ref_int32_x = ref__Ref_5int32(inline271)
    sum__2 = inline272
    var i__3 *ref_int32_x
    var inline268 int32 = 0
    var inline269 *ref_int32_x = ref__Ref_5int32(inline268)
    i__3 = inline269
    Loop_loop208:
    for {
        var t209 int32
        var inline264 int32 = ref_get__Ref_5int32(i__3)
        t209 = inline264
        var t210 bool = t209 < limit__1
        if t210 {
            var cur__4 int32
            var inline262 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline262
            var t211 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t211)
            var t217 bool = cur__4 == 1
            if t217 {
                continue
            } else {
                var mtmp189 Option__int32
                var inline256 bool = cur__4 == 2
                if inline256 {
                    mtmp189 = None{}
                } else {
                    var inline257 int32 = cur__4 + 10
                    var inline258 Option__int32 = Some{
                        _0: inline257,
                    }
                    mtmp189 = inline258
                }
                var jp214 int32
                switch mtmp189.(type) {
                case None:
                    return None{}
                case Some:
                    var x190 int32 = mtmp189.(Some)._0
                    jp214 = x190
                    var t215 int32
                    var inline254 int32 = ref_get__Ref_5int32(sum__2)
                    t215 = inline254
                    var t216 int32 = t215 + jp214
                    ref_set__Ref_5int32(sum__2, t216)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop208
        }
    }
    var t206 int32
    var inline266 int32 = ref_get__Ref_5int32(sum__2)
    t206 = inline266
    var t207 Option__int32 = Some{
        _0: t206,
    }
    return t207
}

func main0() struct{} {
    var t225 Option__int32 = accumulate(2)
    var t226 string
    switch t225.(type) {
    case None:
        t226 = "none"
    case Some:
        var inline287 int32 = t225.(Some)._0
        var inline289 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline287)
        var inline290 string = "some=" + inline289
        t226 = inline290
    default:
        panic("non-exhaustive match")
    }
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline284)
    var t227 Option__int32 = accumulate(4)
    var t228 string
    switch t227.(type) {
    case None:
        t228 = "none"
    case Some:
        var inline279 int32 = t227.(Some)._0
        var inline281 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline279)
        var inline282 string = "some=" + inline281
        t228 = inline282
    default:
        panic("non-exhaustive match")
    }
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline276)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t239 string = _goml_runtime_core_int32_to_string(self__33)
    return t239
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
