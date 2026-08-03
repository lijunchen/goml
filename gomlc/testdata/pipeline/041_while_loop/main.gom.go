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
    var inline226 int32 = 0
    var inline227 *ref_int32_x = ref__Ref_5int32(inline226)
    acc__1 = inline227
    var i__2 *ref_int32_x
    var inline223 int32 = 0
    var inline224 *ref_int32_x = ref__Ref_5int32(inline223)
    i__2 = inline224
    Loop_loop150:
    for {
        var t151 int32
        var inline219 int32 = ref_get__Ref_5int32(i__2)
        t151 = inline219
        var t152 bool = t151 < limit__0
        if t152 {
            var current__3 int32
            var inline217 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline217
            var t153 int32
            var inline215 int32 = ref_get__Ref_5int32(acc__1)
            t153 = inline215
            var t154 int32 = t153 + current__3
            ref_set__Ref_5int32(acc__1, t154)
            var t155 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t155)
            continue
        } else {
            break Loop_loop150
        }
    }
    var inline221 int32 = ref_get__Ref_5int32(acc__1)
    return inline221
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline251 int32 = 0
    var inline252 *ref_int32_x = ref__Ref_5int32(inline251)
    acc__5 = inline252
    var i__6 *ref_int32_x
    var inline248 int32 = 0
    var inline249 *ref_int32_x = ref__Ref_5int32(inline248)
    i__6 = inline249
    var is_even__7 *ref_bool_x
    var inline245 bool = true
    var inline246 *ref_bool_x = ref__Ref_4bool(inline245)
    is_even__7 = inline246
    Loop_loop160:
    for {
        var t161 int32
        var inline241 int32 = ref_get__Ref_5int32(i__6)
        t161 = inline241
        var t162 bool = t161 < limit__4
        if t162 {
            var current__8 int32
            var inline239 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline239
            var t163 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t163)
            var add_now__9 bool
            var inline235 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline235
            var t164 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t164)
            if add_now__9 {
                var t166 int32
                var inline231 int32 = ref_get__Ref_5int32(acc__5)
                t166 = inline231
                var t167 int32 = t166 + current__8
                ref_set__Ref_5int32(acc__5, t167)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop160
        }
    }
    var inline243 int32 = ref_get__Ref_5int32(acc__5)
    return inline243
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline264 string = "sum_to(5)="
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline264)
    _goml_runtime_core_string_print(inline265)
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline261)
    var inline257 string = "sum_even(6)="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline257)
    _goml_runtime_core_string_print(inline258)
    var inline254 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t196 string = _goml_runtime_core_int32_to_string(self__72)
    return t196
}

func main() {
    main0()
}
