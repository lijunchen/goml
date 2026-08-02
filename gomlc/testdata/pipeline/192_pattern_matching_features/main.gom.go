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
        var jp195 int32
        switch value__0.(type) {
        case Left:
            jp195 = 0
        case Right:
            jp195 = 1
        default:
            panic("non-exhaustive match")
        }
        var t196 int32 = shared__2 + jp195
        return t196
    default:
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp200 int32
            switch value__0.(type) {
            case Left:
                jp200 = 0
            case Right:
                jp200 = 1
            default:
                panic("non-exhaustive match")
            }
            var t201 int32 = shared__2 + jp200
            return t201
        default:
            var t202 int32 = missing__int32("")
            return t202
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp214 string
    switch value__4.(type) {
    case Some:
        var x161 int32 = value__4.(Some)._0
        var t255 bool = x161 == 0
        if t255 {
            jp214 = "small"
        } else {
            var t258 bool = x161 == 1
            if t258 {
                jp214 = "small"
            } else {
                var t261 bool = x161 >= 2
                if t261 {
                    var t264 bool = x161 <= 4
                    if t264 {
                        jp214 = "middle"
                    } else {
                        var t267 bool = x161 > 10
                        if t267 {
                            jp214 = "large"
                        } else {
                            jp214 = "other"
                        }
                    }
                } else {
                    var t270 bool = x161 > 10
                    if t270 {
                        jp214 = "large"
                    } else {
                        jp214 = "other"
                    }
                }
            }
        }
    default:
        jp214 = "none"
    }
    var t231 int = vec_len__Vec_5int32(numbers__5)
    var t232 bool = t231 == 0
    var jp216 string
    if t232 {
        jp216 = "empty"
    } else {
        var t235 int = vec_len__Vec_5int32(numbers__5)
        var t236 bool = t235 >= 1
        if t236 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t237 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t237]
            var t240 int
            var inline334 int = len(tail__11)
            t240 = inline334
            var t241 int32 = int32(int(t240))
            var t242 bool
            var inline332 bool = first__10 == t241
            t242 = inline332
            if t242 {
                jp216 = "balanced"
            } else {
                var t245 int = vec_len__Vec_5int32(numbers__5)
                var t246 bool = t245 >= 1
                if t246 {
                    jp216 = "nonempty"
                } else {
                    var t247 string = missing__string("")
                    jp216 = t247
                }
            }
        } else {
            var t250 int = vec_len__Vec_5int32(numbers__5)
            var t251 bool = t250 >= 1
            if t251 {
                jp216 = "nonempty"
            } else {
                var t252 string = missing__string("")
                jp216 = t252
            }
        }
    }
    var t223 int = len(view__6)
    var t224 bool = t223 >= 2
    var jp218 string
    if t224 {
        var first__13 int32 = view__6[0]
        var t225 int = len(view__6)
        var t226 int = t225 - 1
        var t227 int = t226 + 0
        var last__14 int32 = view__6[t227]
        var t230 bool
        var inline336 bool = first__13 == last__14
        t230 = inline336
        if t230 {
            jp218 = "same ends"
        } else {
            jp218 = "different ends"
        }
    } else {
        jp218 = "different ends"
    }
    var t219 string = jp214 + "/"
    var t220 string = t219 + jp216
    var t221 string = t220 + "/"
    var t222 string = t221 + jp218
    return t222
}

func main0() struct{} {
    var x166 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t272 int = array_get__Array_4_3int(values__18, 1)
    var t273 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t272, t273}
    var inline375 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x166)
    _goml_runtime_core_string_println(inline375)
    var t274 int = array_get__Array_2_3int(middle__20, 0)
    var t275 int = first__19 + t274
    var t276 int = t275 + last__21
    var inline372 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t276)
    _goml_runtime_core_string_println(inline372)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t277 int
    var inline370 int = vec_len__Vec_5int32(numbers__22)
    t277 = inline370
    var view__23 []int32 = numbers__22.items[0:t277]
    var t278 Maybe = Some{
        _0: 3,
    }
    var t279 string = describe(t278, numbers__22, view__23)
    var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t279)
    _goml_runtime_core_string_println(inline367)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t280 string = describe(None{}, empty__24, empty_view__25)
    var inline364 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t280)
    _goml_runtime_core_string_println(inline364)
    var t281 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline362 *ref_Maybe_x = ref__Ref_5Maybe(t281)
    state__26 = inline362
    Loop_loop295:
    for {
        var mtmp175 Maybe
        var inline343 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp175 = inline343
        switch mtmp175.(type) {
        case Some:
            var x176 int32 = mtmp175.(Some)._0
            var inline340 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x176)
            _goml_runtime_core_string_println(inline340)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop295
        }
    }
    var x181 int32 = 6
    var inline345 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x181)
    _goml_runtime_core_string_println(inline345)
    var jp285 int32
    var value__30 int32 = 5
    var jp290 int32
    var x185 int32 = 5
    jp290 = x185
    var t291 int32 = value__30 + jp290
    jp285 = t291
    var inline359 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp285)
    _goml_runtime_core_string_println(inline359)
    var t286 Either = Right{
        _0: 11,
    }
    var t287 int32 = unwrap_either(t286)
    var inline356 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t287)
    _goml_runtime_core_string_println(inline356)
    var t288 string
    var inline351 rune = 98
    var inline353 bool = inline351 >= 97
    if inline353 {
        var inline354 bool = inline351 <= 99
        if inline354 {
            t288 = "abc"
        } else {
            t288 = "other"
        }
    } else {
        t288 = "other"
    }
    var inline348 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t288)
    _goml_runtime_core_string_println(inline348)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t325 string = _goml_runtime_core_int32_to_string(self__43)
    return t325
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t328 string = _goml_runtime_core_int_to_string(self__40)
    return t328
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
