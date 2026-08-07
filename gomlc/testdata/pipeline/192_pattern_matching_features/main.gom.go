package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_4_3int(arr [4]int, index int) int {
    return arr[index]
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type ref_Maybe_x struct {
    value Maybe
}

func ref__Ref_5Maybe(value Maybe) *ref_Maybe_x {
    return &ref_Maybe_x{
        value: value,
    }
}

func ref_get__Ref_5Maybe(reference *ref_Maybe_x) Maybe {
    return reference.value
}

func ref_set__Ref_5Maybe(reference *ref_Maybe_x, value Maybe) struct{} {
    reference.value = value
    return struct{}{}
}

func missing__int32(s string) int32 {
    _goml_intrinsic_missing(s)
    var ret int32
    return ret
}

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Pair struct {
    left int32
    right int32
}

type Maybe interface {
    isMaybe()
}

type None struct {}

func (_ None) isMaybe() {}

type Some struct {
    _0 int32
}

func (_ Some) isMaybe() {}

type Either interface {
    isEither()
}

type Left struct {
    _0 int32
}

func (_ Left) isEither() {}

type Right struct {
    _0 int32
}

func (_ Right) isEither() {}

func unwrap_either(value__0 Either) int32 {
    switch value__0.(type) {
    case Left:
        var shared__2 int32 = value__0.(Left)._0
        var jp212 int32
        switch value__0.(type) {
        case Left:
            jp212 = 0
        case Right:
            jp212 = 1
        default:
            panic("non-exhaustive match")
        }
        var t213 int32 = shared__2 + jp212
        return t213
    default:
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp217 int32
            switch value__0.(type) {
            case Left:
                jp217 = 0
            case Right:
                jp217 = 1
            default:
                panic("non-exhaustive match")
            }
            var t218 int32 = shared__2 + jp217
            return t218
        default:
            var t219 int32 = missing__int32("")
            return t219
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp231 string
    switch value__4.(type) {
    case Some:
        var x178 int32 = value__4.(Some)._0
        var t272 bool = x178 == 0
        if t272 {
            jp231 = "small"
        } else {
            var t275 bool = x178 == 1
            if t275 {
                jp231 = "small"
            } else {
                var t278 bool = x178 >= 2
                if t278 {
                    var t281 bool = x178 <= 4
                    if t281 {
                        jp231 = "middle"
                    } else {
                        var t284 bool = x178 > 10
                        if t284 {
                            jp231 = "large"
                        } else {
                            jp231 = "other"
                        }
                    }
                } else {
                    var t287 bool = x178 > 10
                    if t287 {
                        jp231 = "large"
                    } else {
                        jp231 = "other"
                    }
                }
            }
        }
    default:
        jp231 = "none"
    }
    var t248 int = vec_len__Vec_5int32(numbers__5)
    var t249 bool = t248 == 0
    var jp233 string
    if t249 {
        jp233 = "empty"
    } else {
        var t252 int = vec_len__Vec_5int32(numbers__5)
        var t253 bool = t252 >= 1
        if t253 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t254 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t254]
            var t257 int
            var inline351 int = len(tail__11)
            t257 = inline351
            var t258 int32 = int32(int(t257))
            var t259 bool
            var inline349 bool = first__10 == t258
            t259 = inline349
            if t259 {
                jp233 = "balanced"
            } else {
                var t262 int = vec_len__Vec_5int32(numbers__5)
                var t263 bool = t262 >= 1
                if t263 {
                    jp233 = "nonempty"
                } else {
                    var t264 string = missing__string("")
                    jp233 = t264
                }
            }
        } else {
            var t267 int = vec_len__Vec_5int32(numbers__5)
            var t268 bool = t267 >= 1
            if t268 {
                jp233 = "nonempty"
            } else {
                var t269 string = missing__string("")
                jp233 = t269
            }
        }
    }
    var t240 int = len(view__6)
    var t241 bool = t240 >= 2
    var jp235 string
    if t241 {
        var first__13 int32 = view__6[0]
        var t242 int = len(view__6)
        var t243 int = t242 - 1
        var t244 int = t243 + 0
        var last__14 int32 = view__6[t244]
        var t247 bool
        var inline353 bool = first__13 == last__14
        t247 = inline353
        if t247 {
            jp235 = "same ends"
        } else {
            jp235 = "different ends"
        }
    } else {
        jp235 = "different ends"
    }
    var t236 string = jp231 + "/"
    var t237 string = t236 + jp233
    var t238 string = t237 + "/"
    var t239 string = t238 + jp235
    return t239
}

func main0() struct{} {
    var x183 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t289 int = array_get__Array_4_3int(values__18, 1)
    var t290 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t289, t290}
    var inline392 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x183)
    _goml_runtime_core_string_println(inline392)
    var t291 int = array_get__Array_2_3int(middle__20, 0)
    var t292 int = first__19 + t291
    var t293 int = t292 + last__21
    var inline389 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t293)
    _goml_runtime_core_string_println(inline389)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t294 int
    var inline387 int = vec_len__Vec_5int32(numbers__22)
    t294 = inline387
    var view__23 []int32 = numbers__22.items[0:t294]
    var t295 Maybe = Some{
        _0: 3,
    }
    var t296 string = describe(t295, numbers__22, view__23)
    var inline384 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t296)
    _goml_runtime_core_string_println(inline384)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t297 string = describe(None{}, empty__24, empty_view__25)
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t297)
    _goml_runtime_core_string_println(inline381)
    var t298 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline379 *ref_Maybe_x = ref__Ref_5Maybe(t298)
    state__26 = inline379
    Loop_loop312:
    for {
        var mtmp192 Maybe
        var inline360 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp192 = inline360
        switch mtmp192.(type) {
        case Some:
            var x193 int32 = mtmp192.(Some)._0
            var inline357 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x193)
            _goml_runtime_core_string_println(inline357)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop312
        }
    }
    var x198 int32 = 6
    var inline362 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x198)
    _goml_runtime_core_string_println(inline362)
    var jp302 int32
    var value__30 int32 = 5
    var jp307 int32
    var x202 int32 = 5
    jp307 = x202
    var t308 int32 = value__30 + jp307
    jp302 = t308
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp302)
    _goml_runtime_core_string_println(inline376)
    var t303 Either = Right{
        _0: 11,
    }
    var t304 int32 = unwrap_either(t303)
    var inline373 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t304)
    _goml_runtime_core_string_println(inline373)
    var t305 string
    var inline368 rune = 98
    var inline370 bool = inline368 >= 97
    if inline370 {
        var inline371 bool = inline368 <= 99
        if inline371 {
            t305 = "abc"
        } else {
            t305 = "other"
        }
    } else {
        t305 = "other"
    }
    var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t305)
    _goml_runtime_core_string_println(inline365)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t342 string = _goml_runtime_core_int32_to_string(self__72)
    return t342
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t345 string = _goml_runtime_core_int_to_string(self__69)
    return t345
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
