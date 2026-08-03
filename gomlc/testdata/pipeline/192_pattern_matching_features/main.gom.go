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
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp222 int32
            switch value__0.(type) {
            case Left:
                jp222 = 0
            case Right:
                jp222 = 1
            default:
                panic("non-exhaustive match")
            }
            var t223 int32 = shared__2 + jp222
            return t223
        default:
            var t224 int32 = missing__int32("")
            return t224
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp236 string
    switch value__4.(type) {
    case Some:
        var x183 int32 = value__4.(Some)._0
        var t277 bool = x183 == 0
        if t277 {
            jp236 = "small"
        } else {
            var t280 bool = x183 == 1
            if t280 {
                jp236 = "small"
            } else {
                var t283 bool = x183 >= 2
                if t283 {
                    var t286 bool = x183 <= 4
                    if t286 {
                        jp236 = "middle"
                    } else {
                        var t289 bool = x183 > 10
                        if t289 {
                            jp236 = "large"
                        } else {
                            jp236 = "other"
                        }
                    }
                } else {
                    var t292 bool = x183 > 10
                    if t292 {
                        jp236 = "large"
                    } else {
                        jp236 = "other"
                    }
                }
            }
        }
    default:
        jp236 = "none"
    }
    var t253 int = vec_len__Vec_5int32(numbers__5)
    var t254 bool = t253 == 0
    var jp238 string
    if t254 {
        jp238 = "empty"
    } else {
        var t257 int = vec_len__Vec_5int32(numbers__5)
        var t258 bool = t257 >= 1
        if t258 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t259 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t259]
            var t262 int
            var inline356 int = len(tail__11)
            t262 = inline356
            var t263 int32 = int32(int(t262))
            var t264 bool
            var inline354 bool = first__10 == t263
            t264 = inline354
            if t264 {
                jp238 = "balanced"
            } else {
                var t267 int = vec_len__Vec_5int32(numbers__5)
                var t268 bool = t267 >= 1
                if t268 {
                    jp238 = "nonempty"
                } else {
                    var t269 string = missing__string("")
                    jp238 = t269
                }
            }
        } else {
            var t272 int = vec_len__Vec_5int32(numbers__5)
            var t273 bool = t272 >= 1
            if t273 {
                jp238 = "nonempty"
            } else {
                var t274 string = missing__string("")
                jp238 = t274
            }
        }
    }
    var t245 int = len(view__6)
    var t246 bool = t245 >= 2
    var jp240 string
    if t246 {
        var first__13 int32 = view__6[0]
        var t247 int = len(view__6)
        var t248 int = t247 - 1
        var t249 int = t248 + 0
        var last__14 int32 = view__6[t249]
        var t252 bool
        var inline358 bool = first__13 == last__14
        t252 = inline358
        if t252 {
            jp240 = "same ends"
        } else {
            jp240 = "different ends"
        }
    } else {
        jp240 = "different ends"
    }
    var t241 string = jp236 + "/"
    var t242 string = t241 + jp238
    var t243 string = t242 + "/"
    var t244 string = t243 + jp240
    return t244
}

func main0() struct{} {
    var x188 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t294 int = array_get__Array_4_3int(values__18, 1)
    var t295 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t294, t295}
    var inline397 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x188)
    _goml_runtime_core_string_println(inline397)
    var t296 int = array_get__Array_2_3int(middle__20, 0)
    var t297 int = first__19 + t296
    var t298 int = t297 + last__21
    var inline394 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t298)
    _goml_runtime_core_string_println(inline394)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t299 int
    var inline392 int = vec_len__Vec_5int32(numbers__22)
    t299 = inline392
    var view__23 []int32 = numbers__22.items[0:t299]
    var t300 Maybe = Some{
        _0: 3,
    }
    var t301 string = describe(t300, numbers__22, view__23)
    var inline389 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t301)
    _goml_runtime_core_string_println(inline389)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t302 string = describe(None{}, empty__24, empty_view__25)
    var inline386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t302)
    _goml_runtime_core_string_println(inline386)
    var t303 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline384 *ref_Maybe_x = ref__Ref_5Maybe(t303)
    state__26 = inline384
    Loop_loop317:
    for {
        var mtmp197 Maybe
        var inline365 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp197 = inline365
        switch mtmp197.(type) {
        case Some:
            var x198 int32 = mtmp197.(Some)._0
            var inline362 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x198)
            _goml_runtime_core_string_println(inline362)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop317
        }
    }
    var x203 int32 = 6
    var inline367 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x203)
    _goml_runtime_core_string_println(inline367)
    var jp307 int32
    var value__30 int32 = 5
    var jp312 int32
    var x207 int32 = 5
    jp312 = x207
    var t313 int32 = value__30 + jp312
    jp307 = t313
    var inline381 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp307)
    _goml_runtime_core_string_println(inline381)
    var t308 Either = Right{
        _0: 11,
    }
    var t309 int32 = unwrap_either(t308)
    var inline378 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t309)
    _goml_runtime_core_string_println(inline378)
    var t310 string
    var inline373 rune = 98
    var inline375 bool = inline373 >= 97
    if inline375 {
        var inline376 bool = inline373 <= 99
        if inline376 {
            t310 = "abc"
        } else {
            t310 = "other"
        }
    } else {
        t310 = "other"
    }
    var inline370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t310)
    _goml_runtime_core_string_println(inline370)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t347 string = _goml_runtime_core_int32_to_string(self__72)
    return t347
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t350 string = _goml_runtime_core_int_to_string(self__69)
    return t350
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
