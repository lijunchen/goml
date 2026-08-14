package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_Pair struct {
    items []Pair
}

func vec_new__Vec_4Pair() *_goml_vec_Pair {
    return &_goml_vec_Pair{
        items: nil,
    }
}

func vec_push__Vec_4Pair(vec *_goml_vec_Pair, elem Pair) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_4Pair(vec *_goml_vec_Pair, index int) Pair {
    return vec.items[index]
}

func vec_len__Vec_4Pair(vec *_goml_vec_Pair) int {
    return int(len(vec.items))
}

type Pair struct {
    left int
    right int
}

func main0() struct{} {
    var vec_literal__208 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__208, 1)
    var inline355 int = 2
    vec_push__Vec_3int(vec_literal__208, inline355)
    var inline352 int = 3
    vec_push__Vec_3int(vec_literal__208, inline352)
    var t211 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, 2)
    var t212 string
    var inline350 string = _goml_runtime_core_bool_to_string(t211)
    t212 = inline350
    var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline347)
    var t213 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, 9)
    var t214 string
    var inline345 string = _goml_runtime_core_bool_to_string(t213)
    t214 = inline345
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline342)
    var vec_literal__330 *_goml_vec_string
    var inline340 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__330 = inline340
    var inline337 string = "alpha"
    vec_push__Vec_6string(vec_literal__330, inline337)
    var inline334 string = "beta"
    vec_push__Vec_6string(vec_literal__330, inline334)
    var t215 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(vec_literal__330, "beta")
    var t216 string
    var inline332 string = _goml_runtime_core_bool_to_string(t215)
    t216 = inline332
    var inline329 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline329)
    var vec_literal__419 *_goml_vec_Pair
    var inline327 *_goml_vec_Pair = vec_new__Vec_4Pair()
    vec_literal__419 = inline327
    var t217 Pair = Pair{
        left: 1,
        right: 2,
    }
    vec_push__Vec_4Pair(vec_literal__419, t217)
    var t218 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t219 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(vec_literal__419, t218)
    var t220 string
    var inline323 string = _goml_runtime_core_bool_to_string(t219)
    t220 = inline323
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline320)
    var t221 bool
    var inline317 int = 3
    var inline318 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, inline317)
    t221 = inline318
    var t222 string
    var inline315 string = _goml_runtime_core_bool_to_string(t221)
    t222 = inline315
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline312)
    var vec_literal__602 *_goml_vec_string
    var inline310 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__602 = inline310
    var t223 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(vec_literal__602, "x")
    var t224 string
    var inline308 string = _goml_runtime_core_bool_to_string(t223)
    t224 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline305)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__101 int, other__102 int) bool {
    var t228 bool = self__101 == other__102
    return t228
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t231 *_goml_vec_int = vec_new__Vec_3int()
    return t231
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(self__235 *_goml_vec_int, expected__236 int) bool {
    var index__237 int = 0
    Loop_loop240:
    for {
        var t241 int
        var inline361 int = vec_len__Vec_3int(self__235)
        t241 = inline361
        var t242 bool = index__237 < t241
        if t242 {
            var t246 int = vec_get__Vec_3int(self__235, index__237)
            var t247 bool
            var inline359 bool = t246 == expected__236
            t247 = inline359
            if t247 {
                return true
            } else {
                var compound_old148 int = index__237
                var compound_value149 int = 1
                var t244 int = compound_old148 + compound_value149
                index__237 = t244
                continue
            }
        } else {
            break Loop_loop240
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__235 *_goml_vec_string, expected__236 string) bool {
    var index__237 int = 0
    Loop_loop259:
    for {
        var t260 int
        var inline365 int = vec_len__Vec_6string(self__235)
        t260 = inline365
        var t261 bool = index__237 < t260
        if t261 {
            var t265 string = vec_get__Vec_6string(self__235, index__237)
            var t266 bool
            var inline363 bool = t265 == expected__236
            t266 = inline363
            if t266 {
                return true
            } else {
                var compound_old148 int = index__237
                var compound_value149 int = 1
                var t263 int = compound_old148 + compound_value149
                index__237 = t263
                continue
            }
        } else {
            break Loop_loop259
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__235 *_goml_vec_Pair, expected__236 Pair) bool {
    var index__237 int = 0
    Loop_loop275:
    for {
        var t276 int
        var inline376 int = vec_len__Vec_4Pair(self__235)
        t276 = inline376
        var t277 bool = index__237 < t276
        if t277 {
            var t281 Pair = vec_get__Vec_4Pair(self__235, index__237)
            var t282 bool
            var inline368 bool
            var inline372 int = t281.left
            var inline373 int = expected__236.left
            var inline374 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline372, inline373)
            inline368 = inline374
            if inline368 {
                var inline369 int = t281.right
                var inline370 int = expected__236.right
                var inline371 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline369, inline370)
                t282 = inline371
                if t282 {
                    return true
                } else {
                    var compound_old148 int = index__237
                    var compound_value149 int = 1
                    var t279 int = compound_old148 + compound_value149
                    index__237 = t279
                    continue
                }
            } else {
                t282 = false
                if t282 {
                    return true
                } else {
                    var compound_old148 int = index__237
                    var compound_value149 int = 1
                    var t279 int = compound_old148 + compound_value149
                    index__237 = t279
                    continue
                }
            }
        } else {
            break Loop_loop275
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
