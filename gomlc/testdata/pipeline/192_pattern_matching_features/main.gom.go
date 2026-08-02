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
    var retv191 int32
    var match155 Either = value__0
    var whole__1 Either = match155
    var jp193 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp195 int32
        switch whole__1.(type) {
        case Left:
            jp195 = 0
        case Right:
            jp195 = 1
        default:
            panic("non-exhaustive match")
        }
        var t196 int32 = shared__2 + jp195
        jp193 = t196
    default:
        var jp198 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp200 int32
            switch whole__1.(type) {
            case Left:
                jp200 = 0
            case Right:
                jp200 = 1
            default:
                panic("non-exhaustive match")
            }
            var t201 int32 = shared__2 + jp200
            jp198 = t201
        default:
            var t202 int32 = missing__int32("")
            jp198 = t202
        }
        jp193 = jp198
    }
    retv191 = jp193
    return retv191
}

func char_group(value__3 rune) string {
    var retv204 string
    var match160 rune = value__3
    var t207 bool = match160 >= 97
    var jp206 string
    if t207 {
        var t210 bool = match160 <= 99
        var jp209 string
        if t210 {
            jp209 = "abc"
        } else {
            jp209 = "other"
        }
        jp206 = jp209
    } else {
        jp206 = "other"
    }
    retv204 = jp206
    return retv204
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv212 string
    var jp214 string
    switch value__4.(type) {
    case Some:
        var x161 int32 = value__4.(Some)._0
        var n__7 int32 = x161
        var match162 int32 = n__7
        var t255 bool = match162 == 0
        var jp254 string
        if t255 {
            jp254 = "small"
        } else {
            var t258 bool = match162 == 1
            var jp257 string
            if t258 {
                jp257 = "small"
            } else {
                var t261 bool = match162 >= 2
                var jp260 string
                if t261 {
                    var t264 bool = match162 <= 4
                    var jp263 string
                    if t264 {
                        jp263 = "middle"
                    } else {
                        var x__8 int32 = match162
                        var t267 bool = x__8 > 10
                        var jp266 string
                        if t267 {
                            jp266 = "large"
                        } else {
                            jp266 = "other"
                        }
                        jp263 = jp266
                    }
                    jp260 = jp263
                } else {
                    var x__8 int32 = match162
                    var t270 bool = x__8 > 10
                    var jp269 string
                    if t270 {
                        jp269 = "large"
                    } else {
                        jp269 = "other"
                    }
                    jp260 = jp269
                }
                jp257 = jp260
            }
            jp254 = jp257
        }
        jp214 = jp254
    default:
        jp214 = "none"
    }
    var from_if__9 string = jp214
    var match163 *_goml_vec_int32 = numbers__5
    var t231 int = vec_len__Vec_5int32(match163)
    var t232 bool = t231 == 0
    var jp216 string
    if t232 {
        jp216 = "empty"
    } else {
        var t235 int = vec_len__Vec_5int32(match163)
        var t236 bool = t235 >= 1
        var jp234 string
        if t236 {
            var first__10 int32 = vec_get__Vec_5int32(match163, 0)
            var t237 int = vec_len__Vec_5int32(match163)
            var tail__11 []int32 = match163.items[1:t237]
            var t240 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t241 int32 = int32(int(t240))
            var t242 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__10, t241)
            var jp239 string
            if t242 {
                jp239 = "balanced"
            } else {
                var t245 int = vec_len__Vec_5int32(match163)
                var t246 bool = t245 >= 1
                var jp244 string
                if t246 {
                    jp244 = "nonempty"
                } else {
                    var t247 string = missing__string("")
                    jp244 = t247
                }
                jp239 = jp244
            }
            jp234 = jp239
        } else {
            var t250 int = vec_len__Vec_5int32(match163)
            var t251 bool = t250 >= 1
            var jp249 string
            if t251 {
                jp249 = "nonempty"
            } else {
                var t252 string = missing__string("")
                jp249 = t252
            }
            jp234 = jp249
        }
        jp216 = jp234
    }
    var from_vec__12 string = jp216
    var match164 []int32 = view__6
    var t223 int = len(match164)
    var t224 bool = t223 >= 2
    var jp218 string
    if t224 {
        var first__13 int32 = match164[0]
        var t225 int = len(match164)
        var t226 int = t225 - 1
        var t227 int = t226 + 0
        var last__14 int32 = match164[t227]
        var t230 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__13, last__14)
        var jp229 string
        if t230 {
            jp229 = "same ends"
        } else {
            jp229 = "different ends"
        }
        jp218 = jp229
    } else {
        jp218 = "different ends"
    }
    var from_slice__15 string = jp218
    var t219 string = from_if__9 + "/"
    var t220 string = t219 + from_vec__12
    var t221 string = t220 + "/"
    var t222 string = t221 + from_slice__15
    retv212 = t222
    return retv212
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp165 Pair = pair__16
    var x166 int32 = mtmp165.left
    var left__17 int32 = x166
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var mtmp168 [4]int = values__18
    var first__19 int = array_get__Array_4_3int(mtmp168, 0)
    var last__21 int = array_get__Array_4_3int(mtmp168, 3)
    var t272 int = array_get__Array_4_3int(mtmp168, 1)
    var t273 int = array_get__Array_4_3int(mtmp168, 2)
    var middle__20 [2]int = [2]int{t272, t273}
    println__T_int32(left__17)
    var t274 int = array_get__Array_2_3int(middle__20, 0)
    var t275 int = first__19 + t274
    var t276 int = t275 + last__21
    println__T_int(t276)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t277 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t277]
    var t278 Maybe = Some{
        _0: 3,
    }
    var t279 string = describe(t278, numbers__22, view__23)
    println__T_string(t279)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t280 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t280)
    var t281 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t281)
    Loop_loop295:
    for {
        if true {
            var mtmp175 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp175.(type) {
            case Some:
                var x176 int32 = mtmp175.(Some)._0
                var n__27 int32 = x176
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop295
            }
        } else {
            break Loop_loop295
        }
    }
    var mtmp180 Maybe = Some{
        _0: 6,
    }
    switch mtmp180.(type) {
    case Some:
        var x181 int32 = mtmp180.(Some)._0
        var n__28 int32 = x181
        println__T_int32(n__28)
    default:
    }
    var match184 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match184
    var jp285 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp290 int32
        switch whole__29.(type) {
        case None:
            jp290 = 0
        case Some:
            var x185 int32 = whole__29.(Some)._0
            var inner__31 int32 = x185
            jp290 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t291 int32 = value__30 + jp290
        jp285 = t291
    default:
        var jp293 int32
        switch match184.(type) {
        case None:
            jp293 = 0
        default:
            var t294 int32 = missing__int32("")
            jp293 = t294
        }
        jp285 = jp293
    }
    var aliased__32 int32 = jp285
    println__T_int32(aliased__32)
    var t286 Either = Right{
        _0: 11,
    }
    var t287 int32 = unwrap_either(t286)
    println__T_int32(t287)
    var t288 string = char_group(98)
    println__T_string(t288)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv298 int
    var t299 int = len(self__186)
    retv298 = t299
    return retv298
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv301 bool
    var t302 bool = self__65 == other__66
    retv301 = t302
    return retv301
}

func println__T_int32(value__1 int32) struct{} {
    var t304 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t304)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t307 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t307)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv310 int
    var t311 int = vec_len__Vec_5int32(self__137)
    retv310 = t311
    return retv310
}

func println__T_string(value__1 string) struct{} {
    var t313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t313)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__207 Maybe) *ref_Maybe_x {
    var retv316 *ref_Maybe_x
    var t317 *ref_Maybe_x = ref__Ref_5Maybe(value__207)
    retv316 = t317
    return retv316
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__208 *ref_Maybe_x) Maybe {
    var retv319 Maybe
    var t320 Maybe = ref_get__Ref_5Maybe(self__208)
    retv319 = t320
    return retv319
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__209 *ref_Maybe_x, value__210 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv324 string
    var t325 string = _goml_runtime_core_int32_to_string(self__43)
    retv324 = t325
    return retv324
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv327 string
    var t328 string = _goml_runtime_core_int_to_string(self__40)
    retv327 = t328
    return retv327
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv330 string
    retv330 = self__38
    return retv330
}

func main() {
    main0()
}
