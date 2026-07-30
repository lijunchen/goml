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
    var retv144 int32
    var match108 Either = value__0
    var whole__1 Either = match108
    var jp146 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp148 int32
        switch whole__1.(type) {
        case Left:
            jp148 = 0
        case Right:
            jp148 = 1
        default:
            panic("non-exhaustive match")
        }
        var t149 int32 = shared__2 + jp148
        jp146 = t149
    default:
        var jp151 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp153 int32
            switch whole__1.(type) {
            case Left:
                jp153 = 0
            case Right:
                jp153 = 1
            default:
                panic("non-exhaustive match")
            }
            var t154 int32 = shared__2 + jp153
            jp151 = t154
        default:
            var t155 int32 = missing__int32("")
            jp151 = t155
        }
        jp146 = jp151
    }
    retv144 = jp146
    return retv144
}

func char_group(value__3 rune) string {
    var retv157 string
    var match113 rune = value__3
    var t160 bool = match113 >= 97
    var jp159 string
    if t160 {
        var t163 bool = match113 <= 99
        var jp162 string
        if t163 {
            jp162 = "abc"
        } else {
            jp162 = "other"
        }
        jp159 = jp162
    } else {
        jp159 = "other"
    }
    retv157 = jp159
    return retv157
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv165 string
    var jp167 string
    switch value__4.(type) {
    case Some:
        var x114 int32 = value__4.(Some)._0
        var n__7 int32 = x114
        var match115 int32 = n__7
        var t208 bool = match115 == 0
        var jp207 string
        if t208 {
            jp207 = "small"
        } else {
            var t211 bool = match115 == 1
            var jp210 string
            if t211 {
                jp210 = "small"
            } else {
                var t214 bool = match115 >= 2
                var jp213 string
                if t214 {
                    var t217 bool = match115 <= 4
                    var jp216 string
                    if t217 {
                        jp216 = "middle"
                    } else {
                        var x__8 int32 = match115
                        var t220 bool = x__8 > 10
                        var jp219 string
                        if t220 {
                            jp219 = "large"
                        } else {
                            jp219 = "other"
                        }
                        jp216 = jp219
                    }
                    jp213 = jp216
                } else {
                    var x__8 int32 = match115
                    var t223 bool = x__8 > 10
                    var jp222 string
                    if t223 {
                        jp222 = "large"
                    } else {
                        jp222 = "other"
                    }
                    jp213 = jp222
                }
                jp210 = jp213
            }
            jp207 = jp210
        }
        jp167 = jp207
    default:
        jp167 = "none"
    }
    var from_if__9 string = jp167
    var match116 *_goml_vec_int32 = numbers__5
    var t184 int = vec_len__Vec_5int32(match116)
    var t185 bool = t184 == 0
    var jp169 string
    if t185 {
        jp169 = "empty"
    } else {
        var t188 int = vec_len__Vec_5int32(match116)
        var t189 bool = t188 >= 1
        var jp187 string
        if t189 {
            var first__10 int32 = vec_get__Vec_5int32(match116, 0)
            var t190 int = vec_len__Vec_5int32(match116)
            var tail__11 []int32 = match116.items[1:t190]
            var t193 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t194 int32 = int32(int(t193))
            var t195 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__10, t194)
            var jp192 string
            if t195 {
                jp192 = "balanced"
            } else {
                var t198 int = vec_len__Vec_5int32(match116)
                var t199 bool = t198 >= 1
                var jp197 string
                if t199 {
                    jp197 = "nonempty"
                } else {
                    var t200 string = missing__string("")
                    jp197 = t200
                }
                jp192 = jp197
            }
            jp187 = jp192
        } else {
            var t203 int = vec_len__Vec_5int32(match116)
            var t204 bool = t203 >= 1
            var jp202 string
            if t204 {
                jp202 = "nonempty"
            } else {
                var t205 string = missing__string("")
                jp202 = t205
            }
            jp187 = jp202
        }
        jp169 = jp187
    }
    var from_vec__12 string = jp169
    var match117 []int32 = view__6
    var t176 int = len(match117)
    var t177 bool = t176 >= 2
    var jp171 string
    if t177 {
        var first__13 int32 = match117[0]
        var t178 int = len(match117)
        var t179 int = t178 - 1
        var t180 int = t179 + 0
        var last__14 int32 = match117[t180]
        var t183 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__13, last__14)
        var jp182 string
        if t183 {
            jp182 = "same ends"
        } else {
            jp182 = "different ends"
        }
        jp171 = jp182
    } else {
        jp171 = "different ends"
    }
    var from_slice__15 string = jp171
    var t172 string = from_if__9 + "/"
    var t173 string = t172 + from_vec__12
    var t174 string = t173 + "/"
    var t175 string = t174 + from_slice__15
    retv165 = t175
    return retv165
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp118 Pair = pair__16
    var x119 int32 = mtmp118.left
    var left__17 int32 = x119
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var mtmp121 [4]int = values__18
    var first__19 int = array_get__Array_4_3int(mtmp121, 0)
    var last__21 int = array_get__Array_4_3int(mtmp121, 3)
    var t225 int = array_get__Array_4_3int(mtmp121, 1)
    var t226 int = array_get__Array_4_3int(mtmp121, 2)
    var middle__20 [2]int = [2]int{t225, t226}
    println__T_int32(left__17)
    var t227 int = array_get__Array_2_3int(middle__20, 0)
    var t228 int = first__19 + t227
    var t229 int = t228 + last__21
    println__T_int(t229)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t230 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t230]
    var t231 Maybe = Some{
        _0: 3,
    }
    var t232 string = describe(t231, numbers__22, view__23)
    println__T_string(t232)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t233 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t233)
    var t234 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t234)
    Loop_loop248:
    for {
        if true {
            var mtmp128 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp128.(type) {
            case Some:
                var x129 int32 = mtmp128.(Some)._0
                var n__27 int32 = x129
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop248
            }
        } else {
            break Loop_loop248
        }
    }
    var mtmp133 Maybe = Some{
        _0: 6,
    }
    switch mtmp133.(type) {
    case Some:
        var x134 int32 = mtmp133.(Some)._0
        var n__28 int32 = x134
        println__T_int32(n__28)
    default:
    }
    var match137 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match137
    var jp238 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp243 int32
        switch whole__29.(type) {
        case None:
            jp243 = 0
        case Some:
            var x138 int32 = whole__29.(Some)._0
            var inner__31 int32 = x138
            jp243 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t244 int32 = value__30 + jp243
        jp238 = t244
    default:
        var jp246 int32
        switch match137.(type) {
        case None:
            jp246 = 0
        default:
            var t247 int32 = missing__int32("")
            jp246 = t247
        }
        jp238 = jp246
    }
    var aliased__32 int32 = jp238
    println__T_int32(aliased__32)
    var t239 Either = Right{
        _0: 11,
    }
    var t240 int32 = unwrap_either(t239)
    println__T_int32(t240)
    var t241 string = char_group(98)
    println__T_string(t241)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv251 int
    var t252 int = len(self__186)
    retv251 = t252
    return retv251
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv254 bool
    var t255 bool = self__65 == other__66
    retv254 = t255
    return retv254
}

func println__T_int32(value__1 int32) struct{} {
    var t257 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t257)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t260 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv263 int
    var t264 int = vec_len__Vec_5int32(self__137)
    retv263 = t264
    return retv263
}

func println__T_string(value__1 string) struct{} {
    var t266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__207 Maybe) *ref_Maybe_x {
    var retv269 *ref_Maybe_x
    var t270 *ref_Maybe_x = ref__Ref_5Maybe(value__207)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__208 *ref_Maybe_x) Maybe {
    var retv272 Maybe
    var t273 Maybe = ref_get__Ref_5Maybe(self__208)
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__209 *ref_Maybe_x, value__210 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv277 string
    var t278 string = _goml_runtime_core_int32_to_string(self__43)
    retv277 = t278
    return retv277
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv280 string
    var t281 string = _goml_runtime_core_int_to_string(self__40)
    retv280 = t281
    return retv280
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv283 string
    retv283 = self__38
    return retv283
}

func main() {
    main0()
}
