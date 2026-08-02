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
    var inline264 int32 = 0
    var inline265 *ref_int32_x = ref__Ref_5int32(inline264)
    i__2 = inline265
    var total__3 *ref_int32_x
    var inline261 int32 = 0
    var inline262 *ref_int32_x = ref__Ref_5int32(inline261)
    total__3 = inline262
    Loop_loop186:
    for {
        var t187 int32
        var inline257 int32 = ref_get__Ref_5int32(i__2)
        t187 = inline257
        var mtmp155 Option__bool
        var inline253 bool = t187 < 3
        if inline253 {
            var inline254 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp155 = inline254
        } else {
            var inline255 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp155 = inline255
        }
        var jp189 bool
        switch mtmp155.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x156 bool = mtmp155.(Option__bool_Some)._0
            jp189 = x156
            if jp189 {
                var t190 int32
                var inline251 int32 = ref_get__Ref_5int32(total__3)
                t190 = inline251
                var t191 int32
                var inline249 int32 = ref_get__Ref_5int32(i__2)
                t191 = inline249
                var t192 int32 = t190 + t191
                ref_set__Ref_5int32(total__3, t192)
                var t193 int32
                var inline245 int32 = ref_get__Ref_5int32(i__2)
                t193 = inline245
                var t194 int32 = t193 + 1
                ref_set__Ref_5int32(i__2, t194)
                continue
            } else {
                break Loop_loop186
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t184 int32
    var inline259 int32 = ref_get__Ref_5int32(total__3)
    t184 = inline259
    var t185 Option__int32 = Option__int32_Some{
        _0: t184,
    }
    return t185
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline287 int32 = 0
    var inline288 *ref_int32_x = ref__Ref_5int32(inline287)
    i__4 = inline288
    var total__5 *ref_int32_x
    var inline284 int32 = 0
    var inline285 *ref_int32_x = ref__Ref_5int32(inline284)
    total__5 = inline285
    Loop_loop200:
    for {
        var t201 int32
        var inline280 int32 = ref_get__Ref_5int32(i__4)
        t201 = inline280
        var mtmp160 Option__bool
        var inline277 bool = t201 < 2
        if inline277 {
            var inline278 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp160 = inline278
        } else {
            mtmp160 = Option__bool_None{}
        }
        var jp203 bool
        switch mtmp160.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x161 bool = mtmp160.(Option__bool_Some)._0
            jp203 = x161
            if jp203 {
                var t204 int32
                var inline275 int32 = ref_get__Ref_5int32(total__5)
                t204 = inline275
                var t205 int32
                var inline273 int32 = ref_get__Ref_5int32(i__4)
                t205 = inline273
                var t206 int32 = t204 + t205
                ref_set__Ref_5int32(total__5, t206)
                var t207 int32
                var inline269 int32 = ref_get__Ref_5int32(i__4)
                t207 = inline269
                var t208 int32 = t207 + 1
                ref_set__Ref_5int32(i__4, t208)
                continue
            } else {
                break Loop_loop200
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t198 int32
    var inline282 int32 = ref_get__Ref_5int32(total__5)
    t198 = inline282
    var t199 Option__int32 = Option__int32_Some{
        _0: t198,
    }
    return t199
}

func main0() struct{} {
    var t216 Option__int32 = run_some()
    var t217 string
    switch t216.(type) {
    case Option__int32_None:
        t217 = "none"
    case Option__int32_Some:
        var inline303 int32 = t216.(Option__int32_Some)._0
        var inline305 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline303)
        var inline306 string = "some=" + inline305
        t217 = inline306
    default:
        panic("non-exhaustive match")
    }
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline300)
    var t218 Option__int32 = run_none()
    var t219 string
    switch t218.(type) {
    case Option__int32_None:
        t219 = "none"
    case Option__int32_Some:
        var inline295 int32 = t218.(Option__int32_Some)._0
        var inline297 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline295)
        var inline298 string = "some=" + inline297
        t219 = inline298
    default:
        panic("non-exhaustive match")
    }
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline292)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t230 string = _goml_runtime_core_int32_to_string(self__6)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
