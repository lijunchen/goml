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
    var inline245 int32 = 0
    var inline246 *ref_int32_x = ref__Ref_5int32(inline245)
    acc__1 = inline246
    var i__2 *ref_int32_x
    var inline242 int32 = 0
    var inline243 *ref_int32_x = ref__Ref_5int32(inline242)
    i__2 = inline243
    Loop_loop169:
    for {
        var t170 int32
        var inline238 int32 = ref_get__Ref_5int32(i__2)
        t170 = inline238
        var t171 bool = t170 < limit__0
        if t171 {
            var current__3 int32
            var inline236 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline236
            var t172 int32
            var inline234 int32 = ref_get__Ref_5int32(acc__1)
            t172 = inline234
            var t173 int32 = t172 + current__3
            ref_set__Ref_5int32(acc__1, t173)
            var t174 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t174)
            continue
        } else {
            break Loop_loop169
        }
    }
    var inline240 int32 = ref_get__Ref_5int32(acc__1)
    return inline240
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline270 int32 = 0
    var inline271 *ref_int32_x = ref__Ref_5int32(inline270)
    acc__5 = inline271
    var i__6 *ref_int32_x
    var inline267 int32 = 0
    var inline268 *ref_int32_x = ref__Ref_5int32(inline267)
    i__6 = inline268
    var is_even__7 *ref_bool_x
    var inline264 bool = true
    var inline265 *ref_bool_x = ref__Ref_4bool(inline264)
    is_even__7 = inline265
    Loop_loop179:
    for {
        var t180 int32
        var inline260 int32 = ref_get__Ref_5int32(i__6)
        t180 = inline260
        var t181 bool = t180 < limit__4
        if t181 {
            var current__8 int32
            var inline258 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline258
            var t182 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t182)
            var add_now__9 bool
            var inline254 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline254
            var t183 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t183)
            if add_now__9 {
                var t185 int32
                var inline250 int32 = ref_get__Ref_5int32(acc__5)
                t185 = inline250
                var t186 int32 = t185 + current__8
                ref_set__Ref_5int32(acc__5, t186)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop179
        }
    }
    var inline262 int32 = ref_get__Ref_5int32(acc__5)
    return inline262
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline283 string = "sum_to(5)="
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline283)
    _goml_runtime_core_string_print(inline284)
    var inline280 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline280)
    var inline276 string = "sum_even(6)="
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline276)
    _goml_runtime_core_string_print(inline277)
    var inline273 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__43)
    return t215
}

func main() {
    main0()
}
