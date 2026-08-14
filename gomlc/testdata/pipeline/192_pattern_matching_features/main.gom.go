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
        var jp227 int32
        switch value__0.(type) {
        case Left:
            jp227 = 0
        case Right:
            jp227 = 1
        default:
            panic("non-exhaustive match")
        }
        var t228 int32 = shared__2 + jp227
        return t228
    default:
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp232 int32
            switch value__0.(type) {
            case Left:
                jp232 = 0
            case Right:
                jp232 = 1
            default:
                panic("non-exhaustive match")
            }
            var t233 int32 = shared__2 + jp232
            return t233
        default:
            var t234 int32 = missing__int32("")
            return t234
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp246 string
    switch value__4.(type) {
    case Some:
        var x193 int32 = value__4.(Some)._0
        var t287 bool = x193 == 0
        if t287 {
            jp246 = "small"
        } else {
            var t290 bool = x193 == 1
            if t290 {
                jp246 = "small"
            } else {
                var t293 bool = x193 >= 2
                if t293 {
                    var t296 bool = x193 <= 4
                    if t296 {
                        jp246 = "middle"
                    } else {
                        var t299 bool = x193 > 10
                        if t299 {
                            jp246 = "large"
                        } else {
                            jp246 = "other"
                        }
                    }
                } else {
                    var t302 bool = x193 > 10
                    if t302 {
                        jp246 = "large"
                    } else {
                        jp246 = "other"
                    }
                }
            }
        }
    default:
        jp246 = "none"
    }
    var t263 int = vec_len__Vec_5int32(numbers__5)
    var t264 bool = t263 == 0
    var jp248 string
    if t264 {
        jp248 = "empty"
    } else {
        var t267 int = vec_len__Vec_5int32(numbers__5)
        var t268 bool = t267 >= 1
        if t268 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t269 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t269]
            var t272 int
            var inline361 int = len(tail__11)
            t272 = inline361
            var t273 int32 = int32(int(t272))
            var t274 bool = first__10 == t273
            if t274 {
                jp248 = "balanced"
            } else {
                var t277 int = vec_len__Vec_5int32(numbers__5)
                var t278 bool = t277 >= 1
                if t278 {
                    jp248 = "nonempty"
                } else {
                    var t279 string = missing__string("")
                    jp248 = t279
                }
            }
        } else {
            var t282 int = vec_len__Vec_5int32(numbers__5)
            var t283 bool = t282 >= 1
            if t283 {
                jp248 = "nonempty"
            } else {
                var t284 string = missing__string("")
                jp248 = t284
            }
        }
    }
    var t255 int = len(view__6)
    var t256 bool = t255 >= 2
    var jp250 string
    if t256 {
        var first__13 int32 = view__6[0]
        var t257 int = len(view__6)
        var t258 int = t257 - 1
        var t259 int = t258 + 0
        var last__14 int32 = view__6[t259]
        var t262 bool = first__13 == last__14
        if t262 {
            jp250 = "same ends"
        } else {
            jp250 = "different ends"
        }
    } else {
        jp250 = "different ends"
    }
    var t251 string = jp246 + "/"
    var t252 string = t251 + jp248
    var t253 string = t252 + "/"
    var t254 string = t253 + jp250
    return t254
}

func main0() struct{} {
    var x198 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t304 int = array_get__Array_4_3int(values__18, 1)
    var t305 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t304, t305}
    var inline400 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x198)
    _goml_runtime_core_string_println(inline400)
    var t306 int = array_get__Array_2_3int(middle__20, 0)
    var t307 int = first__19 + t306
    var t308 int = t307 + last__21
    var inline397 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t308)
    _goml_runtime_core_string_println(inline397)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t309 int
    var inline395 int = vec_len__Vec_5int32(numbers__22)
    t309 = inline395
    var view__23 []int32 = numbers__22.items[0:t309]
    var t310 Maybe = Some{
        _0: 3,
    }
    var t311 string = describe(t310, numbers__22, view__23)
    var inline392 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t311)
    _goml_runtime_core_string_println(inline392)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t312 string = describe(None{}, empty__24, empty_view__25)
    var inline389 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t312)
    _goml_runtime_core_string_println(inline389)
    var t313 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline387 *ref_Maybe_x = ref__Ref_5Maybe(t313)
    state__26 = inline387
    Loop_loop327:
    for {
        var mtmp207 Maybe
        var inline368 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp207 = inline368
        switch mtmp207.(type) {
        case Some:
            var x208 int32 = mtmp207.(Some)._0
            var inline365 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x208)
            _goml_runtime_core_string_println(inline365)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop327
        }
    }
    var x213 int32 = 6
    var inline370 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x213)
    _goml_runtime_core_string_println(inline370)
    var jp317 int32
    var value__30 int32 = 5
    var jp322 int32
    var x217 int32 = 5
    jp322 = x217
    var t323 int32 = value__30 + jp322
    jp317 = t323
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp317)
    _goml_runtime_core_string_println(inline384)
    var t318 Either = Right{
        _0: 11,
    }
    var t319 int32 = unwrap_either(t318)
    var inline381 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t319)
    _goml_runtime_core_string_println(inline381)
    var t320 string
    var inline376 rune = 98
    var inline378 bool = inline376 >= 97
    if inline378 {
        var inline379 bool = inline376 <= 99
        if inline379 {
            t320 = "abc"
        } else {
            t320 = "other"
        }
    } else {
        t320 = "other"
    }
    var inline373 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t320)
    _goml_runtime_core_string_println(inline373)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t354 string = _goml_runtime_core_int32_to_string(self__70)
    return t354
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t357 string = _goml_runtime_core_int_to_string(self__67)
    return t357
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
