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
        var jp176 int32
        switch value__0.(type) {
        case Left:
            jp176 = 0
        case Right:
            jp176 = 1
        default:
            panic("non-exhaustive match")
        }
        var t177 int32 = shared__2 + jp176
        return t177
    default:
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp181 int32
            switch value__0.(type) {
            case Left:
                jp181 = 0
            case Right:
                jp181 = 1
            default:
                panic("non-exhaustive match")
            }
            var t182 int32 = shared__2 + jp181
            return t182
        default:
            var t183 int32 = missing__int32("")
            return t183
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp195 string
    switch value__4.(type) {
    case Some:
        var x142 int32 = value__4.(Some)._0
        var t236 bool = x142 == 0
        if t236 {
            jp195 = "small"
        } else {
            var t239 bool = x142 == 1
            if t239 {
                jp195 = "small"
            } else {
                var t242 bool = x142 >= 2
                if t242 {
                    var t245 bool = x142 <= 4
                    if t245 {
                        jp195 = "middle"
                    } else {
                        var t248 bool = x142 > 10
                        if t248 {
                            jp195 = "large"
                        } else {
                            jp195 = "other"
                        }
                    }
                } else {
                    var t251 bool = x142 > 10
                    if t251 {
                        jp195 = "large"
                    } else {
                        jp195 = "other"
                    }
                }
            }
        }
    default:
        jp195 = "none"
    }
    var t212 int = vec_len__Vec_5int32(numbers__5)
    var t213 bool = t212 == 0
    var jp197 string
    if t213 {
        jp197 = "empty"
    } else {
        var t216 int = vec_len__Vec_5int32(numbers__5)
        var t217 bool = t216 >= 1
        if t217 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t218 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t218]
            var t221 int
            var inline315 int = len(tail__11)
            t221 = inline315
            var t222 int32 = int32(int(t221))
            var t223 bool
            var inline313 bool = first__10 == t222
            t223 = inline313
            if t223 {
                jp197 = "balanced"
            } else {
                var t226 int = vec_len__Vec_5int32(numbers__5)
                var t227 bool = t226 >= 1
                if t227 {
                    jp197 = "nonempty"
                } else {
                    var t228 string = missing__string("")
                    jp197 = t228
                }
            }
        } else {
            var t231 int = vec_len__Vec_5int32(numbers__5)
            var t232 bool = t231 >= 1
            if t232 {
                jp197 = "nonempty"
            } else {
                var t233 string = missing__string("")
                jp197 = t233
            }
        }
    }
    var t204 int = len(view__6)
    var t205 bool = t204 >= 2
    var jp199 string
    if t205 {
        var first__13 int32 = view__6[0]
        var t206 int = len(view__6)
        var t207 int = t206 - 1
        var t208 int = t207 + 0
        var last__14 int32 = view__6[t208]
        var t211 bool
        var inline317 bool = first__13 == last__14
        t211 = inline317
        if t211 {
            jp199 = "same ends"
        } else {
            jp199 = "different ends"
        }
    } else {
        jp199 = "different ends"
    }
    var t200 string = jp195 + "/"
    var t201 string = t200 + jp197
    var t202 string = t201 + "/"
    var t203 string = t202 + jp199
    return t203
}

func main0() struct{} {
    var x147 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t253 int = array_get__Array_4_3int(values__18, 1)
    var t254 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t253, t254}
    var inline356 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x147)
    _goml_runtime_core_string_println(inline356)
    var t255 int = array_get__Array_2_3int(middle__20, 0)
    var t256 int = first__19 + t255
    var t257 int = t256 + last__21
    var inline353 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t257)
    _goml_runtime_core_string_println(inline353)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t258 int
    var inline351 int = vec_len__Vec_5int32(numbers__22)
    t258 = inline351
    var view__23 []int32 = numbers__22.items[0:t258]
    var t259 Maybe = Some{
        _0: 3,
    }
    var t260 string = describe(t259, numbers__22, view__23)
    var inline348 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline348)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t261 string = describe(None{}, empty__24, empty_view__25)
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t261)
    _goml_runtime_core_string_println(inline345)
    var t262 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline343 *ref_Maybe_x = ref__Ref_5Maybe(t262)
    state__26 = inline343
    Loop_loop276:
    for {
        var mtmp156 Maybe
        var inline324 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp156 = inline324
        switch mtmp156.(type) {
        case Some:
            var x157 int32 = mtmp156.(Some)._0
            var inline321 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x157)
            _goml_runtime_core_string_println(inline321)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop276
        }
    }
    var x162 int32 = 6
    var inline326 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x162)
    _goml_runtime_core_string_println(inline326)
    var jp266 int32
    var value__30 int32 = 5
    var jp271 int32
    var x166 int32 = 5
    jp271 = x166
    var t272 int32 = value__30 + jp271
    jp266 = t272
    var inline340 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp266)
    _goml_runtime_core_string_println(inline340)
    var t267 Either = Right{
        _0: 11,
    }
    var t268 int32 = unwrap_either(t267)
    var inline337 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t268)
    _goml_runtime_core_string_println(inline337)
    var t269 string
    var inline332 rune = 98
    var inline334 bool = inline332 >= 97
    if inline334 {
        var inline335 bool = inline332 <= 99
        if inline335 {
            t269 = "abc"
        } else {
            t269 = "other"
        }
    } else {
        t269 = "other"
    }
    var inline329 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t269)
    _goml_runtime_core_string_println(inline329)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t306 string = _goml_runtime_core_int32_to_string(self__72)
    return t306
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t309 string = _goml_runtime_core_int_to_string(self__69)
    return t309
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
