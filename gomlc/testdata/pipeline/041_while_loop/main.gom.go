package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

func sum_to(limit__0 int32) int32 {
    var acc__1 *ref_int32_x
    var inline262 int32 = 0
    var inline263 *ref_int32_x = ref__Ref_5int32(inline262)
    acc__1 = inline263
    var i__2 *ref_int32_x
    var inline259 int32 = 0
    var inline260 *ref_int32_x = ref__Ref_5int32(inline259)
    i__2 = inline260
    Loop_loop186:
    for {
        var t187 int32
        var inline255 int32 = ref_get__Ref_5int32(i__2)
        t187 = inline255
        var t188 bool = t187 < limit__0
        if t188 {
            var current__3 int32
            var inline253 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline253
            var t189 int32
            var inline251 int32 = ref_get__Ref_5int32(acc__1)
            t189 = inline251
            var t190 int32 = t189 + current__3
            ref_set__Ref_5int32(acc__1, t190)
            var t191 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t191)
            continue
        } else {
            break Loop_loop186
        }
    }
    var inline257 int32 = ref_get__Ref_5int32(acc__1)
    return inline257
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline287 int32 = 0
    var inline288 *ref_int32_x = ref__Ref_5int32(inline287)
    acc__5 = inline288
    var i__6 *ref_int32_x
    var inline284 int32 = 0
    var inline285 *ref_int32_x = ref__Ref_5int32(inline284)
    i__6 = inline285
    var is_even__7 *ref_bool_x
    var inline281 bool = true
    var inline282 *ref_bool_x = ref__Ref_4bool(inline281)
    is_even__7 = inline282
    Loop_loop196:
    for {
        var t197 int32
        var inline277 int32 = ref_get__Ref_5int32(i__6)
        t197 = inline277
        var t198 bool = t197 < limit__4
        if t198 {
            var current__8 int32
            var inline275 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline275
            var t199 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t199)
            var add_now__9 bool
            var inline271 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline271
            var t200 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t200)
            if add_now__9 {
                var t202 int32
                var inline267 int32 = ref_get__Ref_5int32(acc__5)
                t202 = inline267
                var t203 int32 = t202 + current__8
                ref_set__Ref_5int32(acc__5, t203)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop196
        }
    }
    var inline279 int32 = ref_get__Ref_5int32(acc__5)
    return inline279
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline300 string = "sum_to(5)="
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline300)
    _goml_runtime_core_string_print(inline301)
    var inline297 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline297)
    var inline293 string = "sum_even(6)="
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline293)
    _goml_runtime_core_string_print(inline294)
    var inline290 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline290)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__72)
    return t232
}

func main() {
    main0()
}
