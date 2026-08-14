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
    var inline266 int32 = 0
    var inline267 *ref_int32_x = ref__Ref_5int32(inline266)
    sum__2 = inline267
    var i__3 *ref_int32_x
    var inline263 int32 = 0
    var inline264 *ref_int32_x = ref__Ref_5int32(inline263)
    i__3 = inline264
    Loop_loop203:
    for {
        var t204 int32
        var inline259 int32 = ref_get__Ref_5int32(i__3)
        t204 = inline259
        var t205 bool = t204 < limit__1
        if t205 {
            var cur__4 int32
            var inline257 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline257
            var t206 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t206)
            var t212 bool = cur__4 == 1
            if t212 {
                continue
            } else {
                var mtmp184 Option__int32
                var inline251 bool = cur__4 == 2
                if inline251 {
                    mtmp184 = None{}
                } else {
                    var inline252 int32 = cur__4 + 10
                    var inline253 Option__int32 = Some{
                        _0: inline252,
                    }
                    mtmp184 = inline253
                }
                var jp209 int32
                switch mtmp184.(type) {
                case None:
                    return None{}
                case Some:
                    var x185 int32 = mtmp184.(Some)._0
                    jp209 = x185
                    var t210 int32
                    var inline249 int32 = ref_get__Ref_5int32(sum__2)
                    t210 = inline249
                    var t211 int32 = t210 + jp209
                    ref_set__Ref_5int32(sum__2, t211)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop203
        }
    }
    var t201 int32
    var inline261 int32 = ref_get__Ref_5int32(sum__2)
    t201 = inline261
    var t202 Option__int32 = Some{
        _0: t201,
    }
    return t202
}

func main0() struct{} {
    var t220 Option__int32 = accumulate(2)
    var t221 string
    switch t220.(type) {
    case None:
        t221 = "none"
    case Some:
        var inline282 int32 = t220.(Some)._0
        var inline284 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline282)
        var inline285 string = "some=" + inline284
        t221 = inline285
    default:
        panic("non-exhaustive match")
    }
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline279)
    var t222 Option__int32 = accumulate(4)
    var t223 string
    switch t222.(type) {
    case None:
        t223 = "none"
    case Some:
        var inline274 int32 = t222.(Some)._0
        var inline276 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline274)
        var inline277 string = "some=" + inline276
        t223 = inline277
    default:
        panic("non-exhaustive match")
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline271)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t234 string = _goml_runtime_core_int32_to_string(self__33)
    return t234
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
