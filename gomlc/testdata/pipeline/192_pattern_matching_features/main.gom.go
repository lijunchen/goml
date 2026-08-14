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
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
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
            var t229 int32 = missing__int32("")
            return t229
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp241 string
    switch value__4.(type) {
    case Some:
        var x188 int32 = value__4.(Some)._0
        var t282 bool = x188 == 0
        if t282 {
            jp241 = "small"
        } else {
            var t285 bool = x188 == 1
            if t285 {
                jp241 = "small"
            } else {
                var t288 bool = x188 >= 2
                if t288 {
                    var t291 bool = x188 <= 4
                    if t291 {
                        jp241 = "middle"
                    } else {
                        var t294 bool = x188 > 10
                        if t294 {
                            jp241 = "large"
                        } else {
                            jp241 = "other"
                        }
                    }
                } else {
                    var t297 bool = x188 > 10
                    if t297 {
                        jp241 = "large"
                    } else {
                        jp241 = "other"
                    }
                }
            }
        }
    default:
        jp241 = "none"
    }
    var t258 int = vec_len__Vec_5int32(numbers__5)
    var t259 bool = t258 == 0
    var jp243 string
    if t259 {
        jp243 = "empty"
    } else {
        var t262 int = vec_len__Vec_5int32(numbers__5)
        var t263 bool = t262 >= 1
        if t263 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t264 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t264]
            var t267 int
            var inline356 int = len(tail__11)
            t267 = inline356
            var t268 int32 = int32(int(t267))
            var t269 bool = first__10 == t268
            if t269 {
                jp243 = "balanced"
            } else {
                var t272 int = vec_len__Vec_5int32(numbers__5)
                var t273 bool = t272 >= 1
                if t273 {
                    jp243 = "nonempty"
                } else {
                    var t274 string = missing__string("")
                    jp243 = t274
                }
            }
        } else {
            var t277 int = vec_len__Vec_5int32(numbers__5)
            var t278 bool = t277 >= 1
            if t278 {
                jp243 = "nonempty"
            } else {
                var t279 string = missing__string("")
                jp243 = t279
            }
        }
    }
    var t250 int = len(view__6)
    var t251 bool = t250 >= 2
    var jp245 string
    if t251 {
        var first__13 int32 = view__6[0]
        var t252 int = len(view__6)
        var t253 int = t252 - 1
        var t254 int = t253 + 0
        var last__14 int32 = view__6[t254]
        var t257 bool = first__13 == last__14
        if t257 {
            jp245 = "same ends"
        } else {
            jp245 = "different ends"
        }
    } else {
        jp245 = "different ends"
    }
    var t246 string = jp241 + "/"
    var t247 string = t246 + jp243
    var t248 string = t247 + "/"
    var t249 string = t248 + jp245
    return t249
}

func main0() struct{} {
    var x193 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t299 int = array_get__Array_4_3int(values__18, 1)
    var t300 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t299, t300}
    var inline395 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x193)
    _goml_runtime_core_string_println(inline395)
    var t301 int = array_get__Array_2_3int(middle__20, 0)
    var t302 int = first__19 + t301
    var t303 int = t302 + last__21
    var inline392 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t303)
    _goml_runtime_core_string_println(inline392)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t304 int
    var inline390 int = vec_len__Vec_5int32(numbers__22)
    t304 = inline390
    var view__23 []int32 = numbers__22.items[0:t304]
    var t305 Maybe = Some{
        _0: 3,
    }
    var t306 string = describe(t305, numbers__22, view__23)
    var inline387 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t306)
    _goml_runtime_core_string_println(inline387)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t307 string = describe(None{}, empty__24, empty_view__25)
    var inline384 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t307)
    _goml_runtime_core_string_println(inline384)
    var t308 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline382 *ref_Maybe_x = ref__Ref_5Maybe(t308)
    state__26 = inline382
    Loop_loop322:
    for {
        var mtmp202 Maybe
        var inline363 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp202 = inline363
        switch mtmp202.(type) {
        case Some:
            var x203 int32 = mtmp202.(Some)._0
            var inline360 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x203)
            _goml_runtime_core_string_println(inline360)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop322
        }
    }
    var x208 int32 = 6
    var inline365 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x208)
    _goml_runtime_core_string_println(inline365)
    var jp312 int32
    var value__30 int32 = 5
    var jp317 int32
    var x212 int32 = 5
    jp317 = x212
    var t318 int32 = value__30 + jp317
    jp312 = t318
    var inline379 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp312)
    _goml_runtime_core_string_println(inline379)
    var t313 Either = Right{
        _0: 11,
    }
    var t314 int32 = unwrap_either(t313)
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t314)
    _goml_runtime_core_string_println(inline376)
    var t315 string
    var inline371 rune = 98
    var inline373 bool = inline371 >= 97
    if inline373 {
        var inline374 bool = inline371 <= 99
        if inline374 {
            t315 = "abc"
        } else {
            t315 = "other"
        }
    } else {
        t315 = "other"
    }
    var inline368 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t315)
    _goml_runtime_core_string_println(inline368)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t349 string = _goml_runtime_core_int32_to_string(self__70)
    return t349
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t352 string = _goml_runtime_core_int_to_string(self__67)
    return t352
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
