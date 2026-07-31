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
    var retv188 int32
    var match152 Either = value__0
    var whole__1 Either = match152
    var jp190 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp192 int32
        switch whole__1.(type) {
        case Left:
            jp192 = 0
        case Right:
            jp192 = 1
        default:
            panic("non-exhaustive match")
        }
        var t193 int32 = shared__2 + jp192
        jp190 = t193
    default:
        var jp195 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp197 int32
            switch whole__1.(type) {
            case Left:
                jp197 = 0
            case Right:
                jp197 = 1
            default:
                panic("non-exhaustive match")
            }
            var t198 int32 = shared__2 + jp197
            jp195 = t198
        default:
            var t199 int32 = missing__int32("")
            jp195 = t199
        }
        jp190 = jp195
    }
    retv188 = jp190
    return retv188
}

func char_group(value__3 rune) string {
    var retv201 string
    var match157 rune = value__3
    var t204 bool = match157 >= 97
    var jp203 string
    if t204 {
        var t207 bool = match157 <= 99
        var jp206 string
        if t207 {
            jp206 = "abc"
        } else {
            jp206 = "other"
        }
        jp203 = jp206
    } else {
        jp203 = "other"
    }
    retv201 = jp203
    return retv201
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv209 string
    var jp211 string
    switch value__4.(type) {
    case Some:
        var x158 int32 = value__4.(Some)._0
        var n__7 int32 = x158
        var match159 int32 = n__7
        var t252 bool = match159 == 0
        var jp251 string
        if t252 {
            jp251 = "small"
        } else {
            var t255 bool = match159 == 1
            var jp254 string
            if t255 {
                jp254 = "small"
            } else {
                var t258 bool = match159 >= 2
                var jp257 string
                if t258 {
                    var t261 bool = match159 <= 4
                    var jp260 string
                    if t261 {
                        jp260 = "middle"
                    } else {
                        var x__8 int32 = match159
                        var t264 bool = x__8 > 10
                        var jp263 string
                        if t264 {
                            jp263 = "large"
                        } else {
                            jp263 = "other"
                        }
                        jp260 = jp263
                    }
                    jp257 = jp260
                } else {
                    var x__8 int32 = match159
                    var t267 bool = x__8 > 10
                    var jp266 string
                    if t267 {
                        jp266 = "large"
                    } else {
                        jp266 = "other"
                    }
                    jp257 = jp266
                }
                jp254 = jp257
            }
            jp251 = jp254
        }
        jp211 = jp251
    default:
        jp211 = "none"
    }
    var from_if__9 string = jp211
    var match160 *_goml_vec_int32 = numbers__5
    var t228 int = vec_len__Vec_5int32(match160)
    var t229 bool = t228 == 0
    var jp213 string
    if t229 {
        jp213 = "empty"
    } else {
        var t232 int = vec_len__Vec_5int32(match160)
        var t233 bool = t232 >= 1
        var jp231 string
        if t233 {
            var first__10 int32 = vec_get__Vec_5int32(match160, 0)
            var t234 int = vec_len__Vec_5int32(match160)
            var tail__11 []int32 = match160.items[1:t234]
            var t237 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t238 int32 = int32(int(t237))
            var t239 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__10, t238)
            var jp236 string
            if t239 {
                jp236 = "balanced"
            } else {
                var t242 int = vec_len__Vec_5int32(match160)
                var t243 bool = t242 >= 1
                var jp241 string
                if t243 {
                    jp241 = "nonempty"
                } else {
                    var t244 string = missing__string("")
                    jp241 = t244
                }
                jp236 = jp241
            }
            jp231 = jp236
        } else {
            var t247 int = vec_len__Vec_5int32(match160)
            var t248 bool = t247 >= 1
            var jp246 string
            if t248 {
                jp246 = "nonempty"
            } else {
                var t249 string = missing__string("")
                jp246 = t249
            }
            jp231 = jp246
        }
        jp213 = jp231
    }
    var from_vec__12 string = jp213
    var match161 []int32 = view__6
    var t220 int = len(match161)
    var t221 bool = t220 >= 2
    var jp215 string
    if t221 {
        var first__13 int32 = match161[0]
        var t222 int = len(match161)
        var t223 int = t222 - 1
        var t224 int = t223 + 0
        var last__14 int32 = match161[t224]
        var t227 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__13, last__14)
        var jp226 string
        if t227 {
            jp226 = "same ends"
        } else {
            jp226 = "different ends"
        }
        jp215 = jp226
    } else {
        jp215 = "different ends"
    }
    var from_slice__15 string = jp215
    var t216 string = from_if__9 + "/"
    var t217 string = t216 + from_vec__12
    var t218 string = t217 + "/"
    var t219 string = t218 + from_slice__15
    retv209 = t219
    return retv209
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp162 Pair = pair__16
    var x163 int32 = mtmp162.left
    var left__17 int32 = x163
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var mtmp165 [4]int = values__18
    var first__19 int = array_get__Array_4_3int(mtmp165, 0)
    var last__21 int = array_get__Array_4_3int(mtmp165, 3)
    var t269 int = array_get__Array_4_3int(mtmp165, 1)
    var t270 int = array_get__Array_4_3int(mtmp165, 2)
    var middle__20 [2]int = [2]int{t269, t270}
    println__T_int32(left__17)
    var t271 int = array_get__Array_2_3int(middle__20, 0)
    var t272 int = first__19 + t271
    var t273 int = t272 + last__21
    println__T_int(t273)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t274 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t274]
    var t275 Maybe = Some{
        _0: 3,
    }
    var t276 string = describe(t275, numbers__22, view__23)
    println__T_string(t276)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t277 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t277)
    var t278 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t278)
    Loop_loop292:
    for {
        if true {
            var mtmp172 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp172.(type) {
            case Some:
                var x173 int32 = mtmp172.(Some)._0
                var n__27 int32 = x173
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop292
            }
        } else {
            break Loop_loop292
        }
    }
    var mtmp177 Maybe = Some{
        _0: 6,
    }
    switch mtmp177.(type) {
    case Some:
        var x178 int32 = mtmp177.(Some)._0
        var n__28 int32 = x178
        println__T_int32(n__28)
    default:
    }
    var match181 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match181
    var jp282 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp287 int32
        switch whole__29.(type) {
        case None:
            jp287 = 0
        case Some:
            var x182 int32 = whole__29.(Some)._0
            var inner__31 int32 = x182
            jp287 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t288 int32 = value__30 + jp287
        jp282 = t288
    default:
        var jp290 int32
        switch match181.(type) {
        case None:
            jp290 = 0
        default:
            var t291 int32 = missing__int32("")
            jp290 = t291
        }
        jp282 = jp290
    }
    var aliased__32 int32 = jp282
    println__T_int32(aliased__32)
    var t283 Either = Right{
        _0: 11,
    }
    var t284 int32 = unwrap_either(t283)
    println__T_int32(t284)
    var t285 string = char_group(98)
    println__T_string(t285)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv295 int
    var t296 int = len(self__186)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv298 bool
    var t299 bool = self__65 == other__66
    retv298 = t299
    return retv298
}

func println__T_int32(value__1 int32) struct{} {
    var t301 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t304)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv307 int
    var t308 int = vec_len__Vec_5int32(self__137)
    retv307 = t308
    return retv307
}

func println__T_string(value__1 string) struct{} {
    var t310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t310)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__207 Maybe) *ref_Maybe_x {
    var retv313 *ref_Maybe_x
    var t314 *ref_Maybe_x = ref__Ref_5Maybe(value__207)
    retv313 = t314
    return retv313
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__208 *ref_Maybe_x) Maybe {
    var retv316 Maybe
    var t317 Maybe = ref_get__Ref_5Maybe(self__208)
    retv316 = t317
    return retv316
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__209 *ref_Maybe_x, value__210 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv321 string
    var t322 string = _goml_runtime_core_int32_to_string(self__43)
    retv321 = t322
    return retv321
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv324 string
    var t325 string = _goml_runtime_core_int_to_string(self__40)
    retv324 = t325
    return retv324
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv327 string
    retv327 = self__38
    return retv327
}

func main() {
    main0()
}
