package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_strings "strings"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_concat(values *_goml_vec_string) string {
    return _goml_strings.Join(values.items, "")
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_set__Vec_6string(vec *_goml_vec_string, index int, value string) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

func vec_truncate__Vec_6string(vec *_goml_vec_string, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_set__Vec_5uint8(vec *_goml_vec_uint8, index int, value uint8) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_int_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_int_c_char_q_
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type closure_env_inherent_string_string_chars_1 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_inherent_string_string_char_indices_2 struct {
    index_0 *ref_int_x
    self_1 string
}

type Ordering int32

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

type _goml_m_Option_____o_int_c_char_q_ interface {
    is_goml_m_Option_____o_int_c_char_q_()
}

type _goml_m_Option_____o_int_c_char_q__None struct {}

func (_ _goml_m_Option_____o_int_c_char_q__None) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_int_c_char_q__Some struct {
    _0 Tuple2_3int_4char
}

func (_ _goml_m_Option_____o_int_c_char_q__Some) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_char_c_int_q_ interface {
    is_goml_m_Option_____o_char_c_int_q_()
}

type _goml_m_Option_____o_char_c_int_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int_q__None) is_goml_m_Option_____o_char_c_int_q_() {}

type _goml_m_Option_____o_char_c_int_q__Some struct {
    _0 Tuple2_4char_3int
}

func (_ _goml_m_Option_____o_char_c_int_q__Some) is_goml_m_Option_____o_char_c_int_q_() {}

func main0() struct{} {
    var text__0 string = "  Héllo, World!  "
    var t429 string = _goml_m_inherent_i_string_i_string_i_trim(text__0)
    var t430 string = "[" + t429
    var t431 string = t430 + "]"
    println__T_string(t431)
    var t432 string = _goml_m_inherent_i_string_i_string_i_trim__start(text__0)
    var t433 string = "[" + t432
    var t434 string = t433 + "]"
    println__T_string(t434)
    var t435 string = _goml_m_inherent_i_string_i_string_i_trim__end(text__0)
    var t436 string = "[" + t435
    var t437 string = t436 + "]"
    println__T_string(t437)
    var t438 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(text__0, ",")
    var t439 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(t438, "|")
    println__T_string(t439)
    var t440 _goml_m_Option_____o_string_c_string_q_ = _goml_m_inherent_i_string_i_string_i_split__once(text__0, ",")
    var t441 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: "",
        _1: "",
    }
    var t442 Tuple2_6string_6string = _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(t440, t441)
    var t443 string = t442._1
    println__T_string(t443)
    var t444 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_lines(text__0)
    var t445 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t444)
    var t446 string = _goml_m_inherent_i_int_i_int_i_to__string(t445)
    println__T_string(t446)
    var t447 Option__int = _goml_m_inherent_i_string_i_string_i_find(text__0, "World")
    var t448 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t447, -1)
    var t449 string = _goml_m_inherent_i_int_i_int_i_to__string(t448)
    println__T_string(t449)
    var t450 Option__int = _goml_m_inherent_i_string_i_string_i_rfind(text__0, "l")
    var t451 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t450, -1)
    var t452 string = _goml_m_inherent_i_int_i_int_i_to__string(t451)
    println__T_string(t452)
    var t453 Option__int
    var inline1188 string = "lo"
    var inline1189 Option__int = _goml_m_inherent_i_string_i_string_i_find(text__0, inline1188)
    t453 = inline1189
    var t454 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t453, -1)
    var t455 string
    var inline1186 string = _goml_runtime_core_int_to_string(t454)
    t455 = inline1186
    var inline1183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t455)
    _goml_runtime_core_string_println(inline1183)
    var t456 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(text__0, 2, "Hé")
    var t457 string
    var inline1181 string = _goml_runtime_core_bool_to_string(t456)
    t457 = inline1181
    var inline1178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline1178)
    var t458 int = _goml_m_inherent_i_string_i_string_i_char__count(text__0)
    var t459 string
    var inline1176 string = _goml_runtime_core_int_to_string(t458)
    t459 = inline1176
    var inline1173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t459)
    _goml_runtime_core_string_println(inline1173)
    var t460 Option__string = _goml_m_inherent_i_string_i_string_i_slice__chars(text__0, 2, 7)
    var t461 string
    var inline1169 string = "none"
    switch t460.(type) {
    case Option__string_None:
        t461 = inline1169
    case Option__string_Some:
        var inline1170 string = t460.(Option__string_Some)._0
        t461 = inline1170
    default:
        panic("non-exhaustive match")
    }
    var inline1166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline1166)
    var t462 string = _goml_m_inherent_i_string_i_string_i_replace(text__0, "l", "L")
    var inline1163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t462)
    _goml_runtime_core_string_println(inline1163)
    var t463 string = _goml_m_inherent_i_string_i_string_i_repeat("ab", 3)
    var inline1160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t463)
    _goml_runtime_core_string_println(inline1160)
    var t464 bool = _goml_m_inherent_i_string_i_string_i_is__ascii(text__0)
    var t465 string
    var inline1158 string = _goml_runtime_core_bool_to_string(t464)
    t465 = inline1158
    var inline1155 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline1155)
    var t466 bool = _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case("ABC", "abc")
    var t467 string
    var inline1153 string = _goml_runtime_core_bool_to_string(t466)
    t467 = inline1153
    var inline1150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline1150)
    var t468 string = _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase("AbC")
    var inline1147 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline1147)
    var t469 string = _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase("aBc")
    var inline1144 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t469)
    _goml_runtime_core_string_println(inline1144)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t472 string
    t472 = value__1
    _goml_runtime_core_string_println(t472)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_trim(self__94 string) string {
    var t476 string = _goml_m_inherent_i_string_i_string_i_trim__start(self__94)
    var t477 string = _goml_m_inherent_i_string_i_string_i_trim__end(t476)
    return t477
}

func _goml_m_inherent_i_string_i_string_i_trim__start(self__90 string) string {
    var start__91 int = 0
    Loop_loop483:
    for {
        var t488 int
        var inline1194 int = _goml_runtime_core_string_len(self__90)
        t488 = inline1194
        var t489 bool = start__91 < t488
        var jp485 bool
        if t489 {
            var t490 uint8
            var inline1192 uint8 = _goml_runtime_core_string_byte_get(self__90, start__91)
            t490 = inline1192
            var t491 bool = ascii_is_whitespace(t490)
            jp485 = t491
        } else {
            jp485 = false
        }
        if jp485 {
            var compound_old77 int = start__91
            var compound_value78 int = 1
            var t486 int = compound_old77 + compound_value78
            start__91 = t486
            continue
        } else {
            break Loop_loop483
        }
    }
    var t481 int
    var inline1198 int = _goml_runtime_core_string_len(self__90)
    t481 = inline1198
    var inline1196 string = string_byte_slice(self__90, start__91, t481)
    return inline1196
}

func _goml_m_inherent_i_string_i_string_i_trim__end(self__92 string) string {
    var end__93 int
    var inline1205 int = _goml_runtime_core_string_len(self__92)
    end__93 = inline1205
    Loop_loop496:
    for {
        var t501 bool = end__93 > 0
        var jp498 bool
        if t501 {
            var t502 int = end__93 - 1
            var t503 uint8
            var inline1200 uint8 = _goml_runtime_core_string_byte_get(self__92, t502)
            t503 = inline1200
            var t504 bool = ascii_is_whitespace(t503)
            jp498 = t504
        } else {
            jp498 = false
        }
        if jp498 {
            var compound_old81 int = end__93
            var compound_value82 int = 1
            var t499 int = compound_old81 - compound_value82
            end__93 = t499
            continue
        } else {
            break Loop_loop496
        }
    }
    var inline1202 int = 0
    var inline1203 string = string_byte_slice(self__92, inline1202, end__93)
    return inline1203
}

func _goml_m_inherent_i_string_i_string_i_split(self__95 string, separator__96 string) *_goml_vec_string {
    var vec_literal__5514 *_goml_vec_string
    var inline1221 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__5514 = inline1221
    var separator_len__98 int
    var inline1219 int = _goml_runtime_core_string_len(separator__96)
    separator_len__98 = inline1219
    var value_len__99 int
    var inline1217 int = _goml_runtime_core_string_len(self__95)
    value_len__99 = inline1217
    var t514 bool = separator_len__98 == 0
    if t514 {
        vec_push__Vec_6string(vec_literal__5514, self__95)
        return vec_literal__5514
    } else {
        var start__100 int = 0
        Loop_loop_expr509:
        for {
            var mtmp87 Option__int = string_find_from(self__95, separator__96, start__100)
            switch mtmp87.(type) {
            case Option__int_None:
                var t511 string
                var inline1211 string = string_byte_slice(self__95, start__100, value_len__99)
                t511 = inline1211
                vec_push__Vec_6string(vec_literal__5514, t511)
                break Loop_loop_expr509
            case Option__int_Some:
                var x88 int = mtmp87.(Option__int_Some)._0
                var t512 string
                var inline1215 string = string_byte_slice(self__95, start__100, x88)
                t512 = inline1215
                vec_push__Vec_6string(vec_literal__5514, t512)
                var t513 int = x88 + separator_len__98
                start__100 = t513
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        return vec_literal__5514
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t517 int
    var inline1260 int = vec_len__Vec_6string(self__378)
    t517 = inline1260
    var parts__380 *_goml_vec_string
    var inline1258 *_goml_vec_string = vec_with_capacity__Vec_6string(t517)
    parts__380 = inline1258
    var t518 int
    var inline1256 int = vec_len__Vec_6string(self__378)
    t518 = inline1256
    var t519 FnIterator__int
    var inline1250 int = 0
    var inline1251 *ref_int_x = ref__Ref_3int(inline1250)
    var inline1252 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1251,
        end_1: t518,
    }
    var inline1253 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1252)
    }
    var inline1254 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1253)
    t519 = inline1254
    var for_iter349 FnIterator__int
    for_iter349 = t519
    Loop_loop534:
    for {
        var for_next350 Option__int
        var inline1226 func() Option__int = for_iter349.next_fn
        var inline1227 Option__int = inline1226()
        for_next350 = inline1227
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop534
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t536 string = vec_get__Vec_6string(self__378, x351)
            var t537 string
            t537 = t536
            vec_push__Vec_6string(parts__380, t537)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t521 int
    var inline1247 int = vec_len__Vec_6string(parts__380)
    t521 = inline1247
    var t522 int = t521 * 2
    var result__382 *_goml_vec_string
    var inline1245 *_goml_vec_string = vec_with_capacity__Vec_6string(t522)
    result__382 = inline1245
    var t523 int
    var inline1243 int = vec_len__Vec_6string(parts__380)
    t523 = inline1243
    var t524 FnIterator__int
    var inline1237 int = 0
    var inline1238 *ref_int_x = ref__Ref_3int(inline1237)
    var inline1239 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1238,
        end_1: t523,
    }
    var inline1240 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1239)
    }
    var inline1241 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1240)
    t524 = inline1241
    var for_iter353 FnIterator__int
    for_iter353 = t524
    Loop_loop527:
    for {
        var for_next354 Option__int
        var inline1233 func() Option__int = for_iter353.next_fn
        var inline1234 Option__int = inline1233()
        for_next354 = inline1234
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop527
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t532 bool = x355 > 0
            if t532 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t530 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t530)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t526 string = _goml_runtime_core_string_concat(result__382)
    return t526
}

func _goml_m_inherent_i_string_i_string_i_split__once(self__102 string, separator__103 string) _goml_m_Option_____o_string_c_string_q_ {
    var separator_len__104 int
    var inline1271 int = _goml_runtime_core_string_len(separator__103)
    separator_len__104 = inline1271
    var value_len__105 int
    var inline1269 int = _goml_runtime_core_string_len(self__102)
    value_len__105 = inline1269
    var t543 bool = separator_len__104 == 0
    if t543 {
        return _goml_m_Option_____o_string_c_string_q__None{}
    } else {
        var mtmp93 Option__int
        var inline1267 Option__int = string_find_from(self__102, separator__103, 0)
        mtmp93 = inline1267
        switch mtmp93.(type) {
        case Option__int_None:
            return _goml_m_Option_____o_string_c_string_q__None{}
        case Option__int_Some:
            var x94 int = mtmp93.(Option__int_Some)._0
            var t546 string
            var inline1264 int = 0
            var inline1265 string = string_byte_slice(self__102, inline1264, x94)
            t546 = inline1265
            var t547 int = x94 + separator_len__104
            var t548 string
            var inline1262 string = string_byte_slice(self__102, t547, value_len__105)
            t548 = inline1262
            var t549 Tuple2_6string_6string = Tuple2_6string_6string{
                _0: t546,
                _1: t548,
            }
            var t550 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
                _0: t549,
            }
            return t550
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(self__458 _goml_m_Option_____o_string_c_string_q_, fallback__459 Tuple2_6string_6string) Tuple2_6string_6string {
    switch self__458.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return fallback__459
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x387 Tuple2_6string_6string = self__458.(_goml_m_Option_____o_string_c_string_q__Some)._0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_string_i_string_i_lines(self__107 string) *_goml_vec_string {
    var result__108 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(self__107, "\n")
    var t577 bool
    var inline1306 int = vec_len__Vec_6string(result__108)
    var inline1307 bool = inline1306 == 0
    t577 = inline1307
    var t578 bool = !t577
    var jp576 bool
    if t578 {
        var t579 int
        var inline1273 int = vec_len__Vec_6string(result__108)
        t579 = inline1273
        var t580 int = t579 - 1
        var t581 string = vec_get__Vec_6string(result__108, t580)
        var t582 bool = t581 == ""
        jp576 = t582
    } else {
        jp576 = false
    }
    if jp576 {
        var inline1275 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(result__108)
        var inline1276 bool = inline1275 == 0
        if inline1276 {} else {
            var inline1277 int = inline1275 - 1
            vec_get__Vec_6string(result__108, inline1277)
            var inline1279 int = inline1275 - 1
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(result__108, inline1279)
        }
    } else {}
    var t558 int
    var inline1304 int = vec_len__Vec_6string(result__108)
    t558 = inline1304
    var t559 FnIterator__int
    var inline1298 int = 0
    var inline1299 *ref_int_x = ref__Ref_3int(inline1298)
    var inline1300 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1299,
        end_1: t558,
    }
    var inline1301 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1300)
    }
    var inline1302 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1301)
    t559 = inline1302
    var for_iter97 FnIterator__int
    for_iter97 = t559
    Loop_loop561:
    for {
        var for_next98 Option__int
        var inline1294 func() Option__int = for_iter97.next_fn
        var inline1295 Option__int = inline1294()
        for_next98 = inline1295
        switch for_next98.(type) {
        case Option__int_None:
            break Loop_loop561
        case Option__int_Some:
            var x99 int = for_next98.(Option__int_Some)._0
            var line__110 string = vec_get__Vec_6string(result__108, x99)
            var t569 int
            var inline1292 int = _goml_runtime_core_string_len(line__110)
            t569 = inline1292
            var t570 bool = t569 > 0
            var jp565 bool
            if t570 {
                var t571 int
                var inline1285 int = _goml_runtime_core_string_len(line__110)
                t571 = inline1285
                var t572 int = t571 - 1
                var t573 uint8
                var inline1283 uint8 = _goml_runtime_core_string_byte_get(line__110, t572)
                t573 = inline1283
                var t574 bool = t573 == 13
                jp565 = t574
            } else {
                jp565 = false
            }
            if jp565 {
                vec_get__Vec_6string(result__108, x99)
                var t566 int
                var inline1290 int = _goml_runtime_core_string_len(line__110)
                t566 = inline1290
                var t567 int = t566 - 1
                var value103 string
                var inline1287 int = 0
                var inline1288 string = string_byte_slice(line__110, inline1287, t567)
                value103 = inline1288
                vec_set__Vec_6string(result__108, x99, value103)
                continue
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return result__108
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__273 *_goml_vec_string) int {
    var t585 int = vec_len__Vec_6string(self__273)
    return t585
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t588 string = _goml_runtime_core_int_to_string(self__32)
    return t588
}

func _goml_m_inherent_i_string_i_string_i_find(self__69 string, expected__70 string) Option__int {
    var t591 Option__int = string_find_from(self__69, expected__70, 0)
    return t591
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(self__458 Option__int, fallback__459 int) int {
    switch self__458.(type) {
    case Option__int_None:
        return fallback__459
    case Option__int_Some:
        var x387 int = self__458.(Option__int_Some)._0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_string_i_string_i_rfind(self__85 string, expected__86 string) Option__int {
    var value_len__87 int
    var inline1311 int = _goml_runtime_core_string_len(self__85)
    value_len__87 = inline1311
    var expected_len__88 int
    var inline1309 int = _goml_runtime_core_string_len(expected__86)
    expected_len__88 = inline1309
    var t600 bool = expected_len__88 > value_len__87
    if t600 {
        return Option__int_None{}
    } else {
        var start__89 int = value_len__87 - expected_len__88
        Loop_loop602:
        for {
            var t603 bool = start__89 >= 0
            if t603 {
                var t605 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(self__85, start__89, expected__86)
                if t605 {
                    var t606 Option__int = Option__int_Some{
                        _0: start__89,
                    }
                    return t606
                } else {
                    var compound_old73 int = start__89
                    var compound_value74 int = 1
                    var t607 int = compound_old73 - compound_value74
                    start__89 = t607
                    continue
                }
            } else {
                break Loop_loop602
            }
        }
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_starts__with__at(self__63 string, start__64 int, prefix__65 string) bool {
    var value_len__66 int
    var inline1323 int = _goml_runtime_core_string_len(self__63)
    value_len__66 = inline1323
    var prefix_len__67 int
    var inline1321 int = _goml_runtime_core_string_len(prefix__65)
    prefix_len__67 = inline1321
    var t630 bool = start__64 < 0
    var jp627 bool
    if t630 {
        jp627 = true
    } else {
        var t631 bool = start__64 > value_len__66
        jp627 = t631
    }
    var jp617 bool
    if jp627 {
        jp617 = true
    } else {
        var t628 int = value_len__66 - start__64
        var t629 bool = prefix_len__67 > t628
        jp617 = t629
    }
    if jp617 {
        return false
    } else {
        var end__68 int = start__64 + prefix_len__67
        var t624 bool
        var inline1319 bool = string_is_char_boundary(self__63, start__64)
        t624 = inline1319
        var jp621 bool
        if t624 {
            var inline1315 bool = string_is_char_boundary(self__63, end__68)
            jp621 = inline1315
        } else {
            jp621 = false
        }
        if jp621 {
            var t622 string
            var inline1317 string = string_byte_slice(self__63, start__64, end__68)
            t622 = inline1317
            var t623 bool = t622 == prefix__65
            return t623
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_char__count(self__73 string) int {
    var count__74 int = 0
    var t637 FnIterator__char
    var inline1329 *ref_int_x = ref__Ref_3int(0)
    var inline1330 closure_env_inherent_string_string_chars_1 = closure_env_inherent_string_string_chars_1{
        self_0: self__73,
        index_1: inline1329,
    }
    var inline1331 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(inline1330)
    }
    var inline1332 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1331)
    t637 = inline1332
    var for_iter43 FnIterator__char
    for_iter43 = t637
    Loop_loop639:
    for {
        var for_next44 Option__char
        var inline1325 func() Option__char = for_iter43.next_fn
        var inline1326 Option__char = inline1325()
        for_next44 = inline1326
        switch for_next44.(type) {
        case Option__char_None:
            break Loop_loop639
        case Option__char_Some:
            var compound_old46 int = count__74
            var compound_value47 int = 1
            var t641 int = compound_old46 + compound_value47
            count__74 = t641
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return count__74
}

func _goml_m_inherent_i_string_i_string_i_slice__chars(self__75 string, start__76 int, end__77 int) Option__string {
    var t683 bool = start__76 < 0
    var jp648 bool
    if t683 {
        jp648 = true
    } else {
        var t684 bool = end__77 < start__76
        jp648 = t684
    }
    if jp648 {
        return Option__string_None{}
    } else {
        var char_index__78 int = 0
        var t681 bool = start__76 == 0
        var jp650 Option__int
        if t681 {
            var t682 Option__int = Option__int_Some{
                _0: 0,
            }
            jp650 = t682
        } else {
            jp650 = Option__int_None{}
        }
        var start_byte__79 Option__int = jp650
        var t679 bool = end__77 == 0
        var jp652 Option__int
        if t679 {
            var t680 Option__int = Option__int_Some{
                _0: 0,
            }
            jp652 = t680
        } else {
            jp652 = Option__int_None{}
        }
        var end_byte__80 Option__int = jp652
        var t653 _goml_m_FnIterator_____o_int_c_char_q_
        var inline1344 *ref_int_x = ref__Ref_3int(0)
        var inline1345 closure_env_inherent_string_string_char_indices_2 = closure_env_inherent_string_string_char_indices_2{
            index_0: inline1344,
            self_1: self__75,
        }
        var inline1346 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
            return _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(inline1345)
        }
        var inline1347 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline1346)
        t653 = inline1347
        var for_iter50 _goml_m_FnIterator_____o_int_c_char_q_
        for_iter50 = t653
        Loop_loop669:
        for {
            var for_next51 _goml_m_Option_____o_int_c_char_q_
            var inline1334 func() _goml_m_Option_____o_int_c_char_q_ = for_iter50.next_fn
            var inline1335 _goml_m_Option_____o_int_c_char_q_ = inline1334()
            for_next51 = inline1335
            switch for_next51.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop669
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x52 Tuple2_3int_4char = for_next51.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var x54 int = x52._0
                var t677 bool = char_index__78 == start__76
                if t677 {
                    var t678 Option__int = Option__int_Some{
                        _0: x54,
                    }
                    start_byte__79 = t678
                } else {}
                var t675 bool = char_index__78 == end__77
                if t675 {
                    var t676 Option__int = Option__int_Some{
                        _0: x54,
                    }
                    end_byte__80 = t676
                } else {}
                var compound_old60 int = char_index__78
                var compound_value61 int = 1
                var t673 int = compound_old60 + compound_value61
                char_index__78 = t673
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t666 bool = char_index__78 == start__76
        if t666 {
            var t667 int
            var inline1337 int = _goml_runtime_core_string_len(self__75)
            t667 = inline1337
            var t668 Option__int = Option__int_Some{
                _0: t667,
            }
            start_byte__79 = t668
        } else {}
        var t663 bool = char_index__78 == end__77
        if t663 {
            var t664 int
            var inline1339 int = _goml_runtime_core_string_len(self__75)
            t664 = inline1339
            var t665 Option__int = Option__int_Some{
                _0: t664,
            }
            end_byte__80 = t665
        } else {}
        var mtmp68 Tuple2_11Option__int_11Option__int = Tuple2_11Option__int_11Option__int{
            _0: start_byte__79,
            _1: end_byte__80,
        }
        var x69 Option__int = mtmp68._0
        var x70 Option__int = mtmp68._1
        switch x70.(type) {
        case Option__int_Some:
            var x71 int = x70.(Option__int_Some)._0
            switch x69.(type) {
            case Option__int_Some:
                var x72 int = x69.(Option__int_Some)._0
                var t661 string
                var inline1341 string = string_byte_slice(self__75, x72, x71)
                t661 = inline1341
                var t662 Option__string = Option__string_Some{
                    _0: t661,
                }
                return t662
            default:
                return Option__string_None{}
            }
        default:
            return Option__string_None{}
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_replace(self__111 string, expected__112 string, replacement__113 string) string {
    var t693 int
    var inline1365 int = _goml_runtime_core_string_len(expected__112)
    t693 = inline1365
    var t694 bool = t693 == 0
    if t694 {
        return self__111
    } else {
        var vec_literal__7376 *_goml_vec_string
        var inline1363 *_goml_vec_string = vec_new__Vec_6string()
        vec_literal__7376 = inline1363
        var start__115 int = 0
        Loop_loop_expr697:
        for {
            var mtmp106 Option__int = string_find_from(self__111, expected__112, start__115)
            switch mtmp106.(type) {
            case Option__int_None:
                var t699 int
                var inline1353 int = _goml_runtime_core_string_len(self__111)
                t699 = inline1353
                var t700 string
                var inline1351 string = string_byte_slice(self__111, start__115, t699)
                t700 = inline1351
                vec_push__Vec_6string(vec_literal__7376, t700)
                break Loop_loop_expr697
            case Option__int_Some:
                var x107 int = mtmp106.(Option__int_Some)._0
                var t701 string
                var inline1361 string = string_byte_slice(self__111, start__115, x107)
                t701 = inline1361
                vec_push__Vec_6string(vec_literal__7376, t701)
                vec_push__Vec_6string(vec_literal__7376, replacement__113)
                var t702 int
                var inline1355 int = _goml_runtime_core_string_len(expected__112)
                t702 = inline1355
                var t703 int = x107 + t702
                start__115 = t703
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t696 string = _goml_runtime_core_string_concat(vec_literal__7376)
        return t696
    }
}

func _goml_m_inherent_i_string_i_string_i_repeat(self__117 string, count__118 int) string {
    var t716 bool = count__118 <= 0
    var jp709 bool
    if t716 {
        jp709 = true
    } else {
        var t717 int
        var inline1367 int = _goml_runtime_core_string_len(self__117)
        t717 = inline1367
        var t718 bool = t717 == 0
        jp709 = t718
    }
    if jp709 {
        return ""
    } else {
        var parts__119 *_goml_vec_string
        var inline1381 *_goml_vec_string = vec_with_capacity__Vec_6string(count__118)
        parts__119 = inline1381
        var t710 FnIterator__int
        var inline1375 int = 0
        var inline1376 *ref_int_x = ref__Ref_3int(inline1375)
        var inline1377 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1376,
            end_1: count__118,
        }
        var inline1378 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1377)
        }
        var inline1379 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1378)
        t710 = inline1379
        var for_iter113 FnIterator__int
        for_iter113 = t710
        Loop_loop713:
        for {
            var for_next114 Option__int
            var inline1371 func() Option__int = for_iter113.next_fn
            var inline1372 Option__int = inline1371()
            for_next114 = inline1372
            switch for_next114.(type) {
            case Option__int_None:
                break Loop_loop713
            case Option__int_Some:
                vec_push__Vec_6string(parts__119, self__117)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t712 string = _goml_runtime_core_string_concat(parts__119)
        return t712
    }
}

func _goml_m_inherent_i_string_i_string_i_is__ascii(self__120 string) bool {
    var t721 int
    var inline1395 int = _goml_runtime_core_string_len(self__120)
    t721 = inline1395
    var t722 FnIterator__int
    var inline1389 int = 0
    var inline1390 *ref_int_x = ref__Ref_3int(inline1389)
    var inline1391 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1390,
        end_1: t721,
    }
    var inline1392 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1391)
    }
    var inline1393 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1392)
    t722 = inline1393
    var for_iter117 FnIterator__int
    for_iter117 = t722
    Loop_loop724:
    for {
        var for_next118 Option__int
        var inline1385 func() Option__int = for_iter117.next_fn
        var inline1386 Option__int = inline1385()
        for_next118 = inline1386
        switch for_next118.(type) {
        case Option__int_None:
            break Loop_loop724
        case Option__int_Some:
            var x119 int = for_next118.(Option__int_Some)._0
            var t727 uint8
            var inline1383 uint8 = _goml_runtime_core_string_byte_get(self__120, x119)
            t727 = inline1383
            var t728 bool = t727 > 127
            if t728 {
                return false
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return true
}

func _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case(self__122 string, other__123 string) bool {
    var t733 int
    var inline1419 int = _goml_runtime_core_string_len(self__122)
    t733 = inline1419
    var t734 int
    var inline1417 int = _goml_runtime_core_string_len(other__123)
    t734 = inline1417
    var t735 bool = t733 != t734
    if t735 {
        return false
    } else {
        var t736 int
        var inline1415 int = _goml_runtime_core_string_len(self__122)
        t736 = inline1415
        var t737 FnIterator__int
        var inline1409 int = 0
        var inline1410 *ref_int_x = ref__Ref_3int(inline1409)
        var inline1411 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1410,
            end_1: t736,
        }
        var inline1412 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1411)
        }
        var inline1413 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1412)
        t737 = inline1413
        var for_iter121 FnIterator__int
        for_iter121 = t737
        Loop_loop739:
        for {
            var for_next122 Option__int
            var inline1405 func() Option__int = for_iter121.next_fn
            var inline1406 Option__int = inline1405()
            for_next122 = inline1406
            switch for_next122.(type) {
            case Option__int_None:
                break Loop_loop739
            case Option__int_Some:
                var x123 int = for_next122.(Option__int_Some)._0
                var t742 uint8
                var inline1403 uint8 = _goml_runtime_core_string_byte_get(self__122, x123)
                t742 = inline1403
                var t743 uint8
                var inline1401 uint8 = _goml_runtime_core_string_byte_get(other__123, x123)
                t743 = inline1401
                var t744 bool
                var inline1397 uint8 = ascii_to_lowercase(t742)
                var inline1398 uint8 = ascii_to_lowercase(t743)
                var inline1399 bool = inline1397 == inline1398
                t744 = inline1399
                var t745 bool = !t744
                if t745 {
                    return false
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        return true
    }
}

func _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase(self__125 string) string {
    var values__126 *_goml_vec_uint8
    var inline1437 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__125)
    values__126 = inline1437
    var t748 int
    var inline1435 int = vec_len__Vec_5uint8(values__126)
    t748 = inline1435
    var t749 FnIterator__int
    var inline1429 int = 0
    var inline1430 *ref_int_x = ref__Ref_3int(inline1429)
    var inline1431 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1430,
        end_1: t748,
    }
    var inline1432 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1431)
    }
    var inline1433 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1432)
    t749 = inline1433
    var for_iter125 FnIterator__int
    for_iter125 = t749
    var inline1422 uint8 = 97 - 65
    Loop_loop751:
    for {
        var for_next126 Option__int
        var inline1425 func() Option__int = for_iter125.next_fn
        var inline1426 Option__int = inline1425()
        for_next126 = inline1426
        switch for_next126.(type) {
        case Option__int_None:
            break Loop_loop751
        case Option__int_Some:
            var x127 int = for_next126.(Option__int_Some)._0
            vec_get__Vec_5uint8(values__126, x127)
            var t753 uint8 = vec_get__Vec_5uint8(values__126, x127)
            var value131 uint8
            var inline1421 bool = ascii_is_uppercase(t753)
            if inline1421 {
                var inline1423 uint8 = t753 + inline1422
                value131 = inline1423
            } else {
                value131 = t753
            }
            vec_set__Vec_5uint8(values__126, x127, value131)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var mtmp134 Tuple2_4bool_6string = string_from_utf8(values__126)
    var x136 string = mtmp134._1
    return x136
}

func _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase(self__129 string) string {
    var values__130 *_goml_vec_uint8
    var inline1455 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__129)
    values__130 = inline1455
    var t757 int
    var inline1453 int = vec_len__Vec_5uint8(values__130)
    t757 = inline1453
    var t758 FnIterator__int
    var inline1447 int = 0
    var inline1448 *ref_int_x = ref__Ref_3int(inline1447)
    var inline1449 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1448,
        end_1: t757,
    }
    var inline1450 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1449)
    }
    var inline1451 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1450)
    t758 = inline1451
    var for_iter137 FnIterator__int
    for_iter137 = t758
    var inline1440 uint8 = 97 - 65
    Loop_loop760:
    for {
        var for_next138 Option__int
        var inline1443 func() Option__int = for_iter137.next_fn
        var inline1444 Option__int = inline1443()
        for_next138 = inline1444
        switch for_next138.(type) {
        case Option__int_None:
            break Loop_loop760
        case Option__int_Some:
            var x139 int = for_next138.(Option__int_Some)._0
            vec_get__Vec_5uint8(values__130, x139)
            var t762 uint8 = vec_get__Vec_5uint8(values__130, x139)
            var value143 uint8
            var inline1439 bool = ascii_is_lowercase(t762)
            if inline1439 {
                var inline1441 uint8 = t762 - inline1440
                value143 = inline1441
            } else {
                value143 = t762
            }
            vec_set__Vec_5uint8(values__130, x139, value143)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var mtmp146 Tuple2_4bool_6string = string_from_utf8(values__130)
    var x148 string = mtmp146._1
    return x148
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t768 int = _goml_runtime_core_string_len(self__36)
    return t768
}

func ascii_is_whitespace(value__140 uint8) bool {
    var t785 bool = value__140 == 9
    var jp783 bool
    if t785 {
        jp783 = true
    } else {
        var t786 bool = value__140 == 10
        jp783 = t786
    }
    var jp780 bool
    if jp783 {
        jp780 = true
    } else {
        var t784 bool = value__140 == 11
        jp780 = t784
    }
    var jp777 bool
    if jp780 {
        jp777 = true
    } else {
        var t781 bool = value__140 == 12
        jp777 = t781
    }
    var jp774 bool
    if jp777 {
        jp774 = true
    } else {
        var t778 bool = value__140 == 13
        jp774 = t778
    }
    if jp774 {
        return true
    } else {
        var t775 bool = value__140 == 32
        return t775
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t789 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t789
}

func string_find_from(value__133 string, expected__134 string, start__135 int) Option__int {
    var value_len__136 int
    var inline1466 int = _goml_runtime_core_string_len(value__133)
    value_len__136 = inline1466
    var expected_len__137 int
    var inline1464 int = _goml_runtime_core_string_len(expected__134)
    expected_len__137 = inline1464
    var t820 bool = start__135 < 0
    var jp803 bool
    if t820 {
        jp803 = true
    } else {
        var t821 bool = start__135 > value_len__136
        jp803 = t821
    }
    if jp803 {
        return Option__int_None{}
    } else {
        var t806 bool = expected_len__137 == 0
        if t806 {
            var t807 Option__int = Option__int_Some{
                _0: start__135,
            }
            return t807
        } else {
            var t810 int = value_len__136 - start__135
            var t811 bool = expected_len__137 > t810
            if t811 {
                return Option__int_None{}
            } else {
                var limit__138 int = value_len__136 - expected_len__137
                var index__139 int = start__135
                Loop_loop813:
                for {
                    var t814 bool = index__139 <= limit__138
                    if t814 {
                        var t816 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(value__133, index__139, expected__134)
                        if t816 {
                            var t817 Option__int = Option__int_Some{
                                _0: index__139,
                            }
                            return t817
                        } else {
                            var compound_old149 int = index__139
                            var compound_value150 int = 1
                            var t818 int = compound_old149 + compound_value150
                            index__139 = t818
                            continue
                        }
                    } else {
                        break Loop_loop813
                    }
                }
                return Option__int_None{}
            }
        }
    }
}

func ascii_to_lowercase(value__143 uint8) uint8 {
    var t888 bool
    var inline1486 bool = value__143 >= 65
    if inline1486 {
        var inline1487 bool = value__143 <= 90
        t888 = inline1487
    } else {
        t888 = false
    }
    if t888 {
        var t889 uint8 = 97 - 65
        var t890 uint8 = value__143 + t889
        return t890
    } else {
        return value__143
    }
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop895:
    for {
        var t896 int
        var inline1489 int = _goml_runtime_core_string_len(x12)
        t896 = inline1489
        var t897 bool = index__26 < t896
        if t897 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t899 int = compound_old17 + x16
                index__26 = t899
                continue
            } else {
                var t901 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t901
            }
        } else {
            break Loop_loop895
        }
    }
    var t894 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t894
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t917 bool = string_is_char_boundary(value__21, start__22)
    var jp914 bool
    if t917 {
        var t918 bool = string_is_char_boundary(value__21, end__23)
        jp914 = t918
    } else {
        jp914 = false
    }
    if jp914 {
        var t915 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t915
    } else {
        var t916 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t916
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t921 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t921
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(self__278 *_goml_vec_string, len__279 int) struct{} {
    vec_truncate__Vec_6string(self__278, len__279)
    return struct{}{}
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t937 bool = index__16 < 0
    var jp929 bool
    if t937 {
        jp929 = true
    } else {
        var t938 int
        var inline1494 int = _goml_runtime_core_string_len(value__15)
        t938 = inline1494
        var t939 bool = index__16 > t938
        jp929 = t939
    }
    if jp929 {
        return false
    } else {
        var t932 int
        var inline1498 int = _goml_runtime_core_string_len(value__15)
        t932 = inline1498
        var t933 bool = index__16 == t932
        if t933 {
            return true
        } else {
            var t934 uint8
            var inline1496 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t934 = inline1496
            var t935_rhs uint8 = 192
            var t935 uint8 = t934 & t935_rhs
            var t936 bool = t935 != 128
            return t936
        }
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__254 func() Option__char) FnIterator__char {
    var t948 FnIterator__char = FnIterator__char{
        next_fn: next_fn__254,
    }
    return t948
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__254 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t951 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__254,
    }
    return t951
}

func ascii_is_uppercase(value__141 uint8) bool {
    var t956 bool = value__141 >= 65
    if t956 {
        var t957 bool = value__141 <= 90
        return t957
    } else {
        return false
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1076 bool = index__6 < 0
    var jp1074 bool
    if t1076 {
        jp1074 = true
    } else {
        var t1077 bool = index__6 >= length__7
        jp1074 = t1077
    }
    if jp1074 {
        var inline1500 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1500
    } else {
        var t961 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t961))
        var t964 bool = first__8 < 128
        if t964 {
            var inline1502 int = 1
            var inline1503 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1503.(type) {
            case Option__char_None:
                var inline1504 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1504
            case Option__char_Some:
                var inline1505 rune = inline1503.(Option__char_Some)._0
                var inline1507 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1505,
                    _2: inline1502,
                }
                return inline1507
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t968 bool = first__8 < 194
            if t968 {
                var inline1509 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1509
            } else {
                var t972 bool = first__8 < 224
                if t972 {
                    var t985 int = length__7 - index__6
                    var t986 bool = t985 < 2
                    if t986 {
                        var inline1511 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1511
                    } else {
                        var t974 int = index__6 + 1
                        var t975 uint8
                        var inline1525 uint8 = _goml_runtime_core_string_byte_get(value__5, t974)
                        t975 = inline1525
                        var second__9 uint32 = uint32(uint8(t975))
                        var t978 bool
                        var inline1522 bool = second__9 < 128
                        if inline1522 {
                            t978 = true
                        } else {
                            var inline1523 bool = second__9 > 191
                            t978 = inline1523
                        }
                        if t978 {
                            var inline1513 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1513
                        } else {
                            var t980_rhs uint32 = 31
                            var t980 uint32 = first__8 & t980_rhs
                            var t981_rhs int = 6
                            var t981 uint32 = t980 << t981_rhs
                            var t982_rhs uint32 = 63
                            var t982 uint32 = second__9 & t982_rhs
                            var t983 uint32 = t981 | t982
                            var inline1515 int = 2
                            var inline1516 Option__char = __goml_builtin_char_from_uint32(t983)
                            switch inline1516.(type) {
                            case Option__char_None:
                                var inline1517 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1517
                            case Option__char_Some:
                                var inline1518 rune = inline1516.(Option__char_Some)._0
                                var inline1520 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1518,
                                    _2: inline1515,
                                }
                                return inline1520
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t990 bool = first__8 < 240
                    if t990 {
                        var t1023 int = length__7 - index__6
                        var t1024 bool = t1023 < 3
                        if t1024 {
                            var inline1527 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1527
                        } else {
                            var t992 int = index__6 + 1
                            var t993 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t992)
                            var second__10 uint32 = uint32(uint8(t993))
                            var t994 int = index__6 + 2
                            var t995 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t994)
                            var third__11 uint32 = uint32(uint8(t995))
                            var t1021 bool = utf8_invalid_continuation(second__10)
                            var jp1016 bool
                            if t1021 {
                                jp1016 = true
                            } else {
                                var inline1529 bool = third__11 < 128
                                if inline1529 {
                                    jp1016 = true
                                } else {
                                    var inline1530 bool = third__11 > 191
                                    jp1016 = inline1530
                                }
                            }
                            var jp1010 bool
                            if jp1016 {
                                jp1010 = true
                            } else {
                                var t1019 bool = first__8 == 224
                                if t1019 {
                                    var t1020 bool = second__10 < 160
                                    jp1010 = t1020
                                } else {
                                    jp1010 = false
                                }
                            }
                            var jp999 bool
                            if jp1010 {
                                jp999 = true
                            } else {
                                var t1013 bool = first__8 == 237
                                if t1013 {
                                    var t1014 bool = second__10 >= 160
                                    jp999 = t1014
                                } else {
                                    jp999 = false
                                }
                            }
                            if jp999 {
                                var inline1532 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1532
                            } else {
                                var t1001_rhs uint32 = 15
                                var t1001 uint32 = first__8 & t1001_rhs
                                var t1002_rhs int = 12
                                var t1002 uint32 = t1001 << t1002_rhs
                                var t1003_rhs uint32 = 63
                                var t1003 uint32 = second__10 & t1003_rhs
                                var t1004_rhs int = 6
                                var t1004 uint32 = t1003 << t1004_rhs
                                var t1005 uint32 = t1002 | t1004
                                var t1006_rhs uint32 = 63
                                var t1006 uint32 = third__11 & t1006_rhs
                                var t1007 uint32 = t1005 | t1006
                                var inline1534 int = 3
                                var inline1535 Option__char = __goml_builtin_char_from_uint32(t1007)
                                switch inline1535.(type) {
                                case Option__char_None:
                                    var inline1536 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1536
                                case Option__char_Some:
                                    var inline1537 rune = inline1535.(Option__char_Some)._0
                                    var inline1539 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1537,
                                        _2: inline1534,
                                    }
                                    return inline1539
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1028 bool = first__8 < 245
                        if t1028 {
                            var t1069 int = length__7 - index__6
                            var t1070 bool = t1069 < 4
                            if t1070 {
                                var t1071 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1071
                            } else {
                                var t1030 int = index__6 + 1
                                var t1031 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1030)
                                var second__12 uint32 = uint32(uint8(t1031))
                                var t1032 int = index__6 + 2
                                var t1033 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1032)
                                var third__13 uint32 = uint32(uint8(t1033))
                                var t1034 int = index__6 + 3
                                var t1035 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1034)
                                var fourth__14 uint32 = uint32(uint8(t1035))
                                var t1067 bool = utf8_invalid_continuation(second__12)
                                var jp1065 bool
                                if t1067 {
                                    jp1065 = true
                                } else {
                                    var t1068 bool = utf8_invalid_continuation(third__13)
                                    jp1065 = t1068
                                }
                                var jp1059 bool
                                if jp1065 {
                                    jp1059 = true
                                } else {
                                    var t1066 bool = utf8_invalid_continuation(fourth__14)
                                    jp1059 = t1066
                                }
                                var jp1053 bool
                                if jp1059 {
                                    jp1053 = true
                                } else {
                                    var t1062 bool = first__8 == 240
                                    if t1062 {
                                        var t1063 bool = second__12 < 144
                                        jp1053 = t1063
                                    } else {
                                        jp1053 = false
                                    }
                                }
                                var jp1039 bool
                                if jp1053 {
                                    jp1039 = true
                                } else {
                                    var t1056 bool = first__8 == 244
                                    if t1056 {
                                        var t1057 bool = second__12 > 143
                                        jp1039 = t1057
                                    } else {
                                        jp1039 = false
                                    }
                                }
                                if jp1039 {
                                    var t1040 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1040
                                } else {
                                    var t1041_rhs uint32 = 7
                                    var t1041 uint32 = first__8 & t1041_rhs
                                    var t1042_rhs int = 18
                                    var t1042 uint32 = t1041 << t1042_rhs
                                    var t1043_rhs uint32 = 63
                                    var t1043 uint32 = second__12 & t1043_rhs
                                    var t1044_rhs int = 12
                                    var t1044 uint32 = t1043 << t1044_rhs
                                    var t1045 uint32 = t1042 | t1044
                                    var t1046_rhs uint32 = 63
                                    var t1046 uint32 = third__13 & t1046_rhs
                                    var t1047_rhs int = 6
                                    var t1047 uint32 = t1046 << t1047_rhs
                                    var t1048 uint32 = t1045 | t1047
                                    var t1049_rhs uint32 = 63
                                    var t1049 uint32 = fourth__14 & t1049_rhs
                                    var t1050 uint32 = t1048 | t1049
                                    var t1051 Tuple3_4bool_4char_3int = utf8_valid_decode(t1050, 4)
                                    return t1051
                                }
                            }
                        } else {
                            var t1072 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1072
                        }
                    }
                }
            }
        }
    }
}

func ascii_is_lowercase(value__142 uint8) bool {
    var t1082 bool = value__142 >= 97
    if t1082 {
        var t1083 bool = value__142 <= 122
        return t1083
    } else {
        return false
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1086 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1086
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1607 rune
    var inline1543 bool = utf8_valid_scalar(value__0)
    if inline1543 {
        var inline1544 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1545 rune = inline1544._1
        commute_field1607 = inline1545
        var t1092 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1607,
            _2: width__1,
        }
        return t1092
    } else {
        var inline1541 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1541
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1097 bool = value__3 < 128
    if t1097 {
        return true
    } else {
        var t1098 bool = value__3 > 191
        return t1098
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1103 bool
    var inline1549 bool = value__30 <= 1114111
    if inline1549 {
        var inline1550 bool = value__30 >= 55296
        var inline1552 bool
        if inline1550 {
            var inline1554 bool = value__30 <= 57343
            inline1552 = inline1554
        } else {
            inline1552 = false
        }
        var inline1553 bool = !inline1552
        t1103 = inline1553
    } else {
        t1103 = false
    }
    if t1103 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1104 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1104
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1109 bool = value__4 <= 1114111
    if t1109 {
        var t1113 bool = value__4 >= 55296
        var jp1111 bool
        if t1113 {
            var t1114 bool = value__4 <= 57343
            jp1111 = t1114
        } else {
            jp1111 = false
        }
        var t1112 bool = !jp1111
        return t1112
    } else {
        return false
    }
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env425 closure_env_goml_builtin_range_0) Option__int {
    var current__496 *ref_int_x = env425.current_0
    var end__495 int = env425.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t1125 bool = value__497 < end__495
    if t1125 {
        var t1126 int = value__497 + 1
        ref_set__Ref_3int(current__496, t1126)
        var t1127 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t1127
    } else {
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(env426 closure_env_inherent_string_string_chars_1) Option__char {
    var self__52 string = env426.self_0
    var index__53 *ref_int_x = env426.index_1
    var t1130 int = ref_get__Ref_3int(index__53)
    var commute_field1610 Tuple2_4char_3int
    var inline1556 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t1130)
    var inline1557 bool = inline1556._0
    var inline1558 rune = inline1556._1
    var inline1559 int = inline1556._2
    if inline1557 {
        var inline1563 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1558,
            _1: inline1559,
        }
        commute_field1610 = inline1563
        var x32 rune = commute_field1610._0
        var x33 int = commute_field1610._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t1133 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t1133)
        var t1135 Option__char = Option__char_Some{
            _0: x32,
        }
        return t1135
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(env427 closure_env_inherent_string_string_char_indices_2) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env427.index_0
    var self__57 string = env427.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1613 Tuple2_4char_3int
    var inline1566 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1567 bool = inline1566._0
    var inline1568 rune = inline1566._1
    var inline1569 int = inline1566._2
    if inline1567 {
        var inline1573 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1568,
            _1: inline1569,
        }
        commute_field1613 = inline1573
        var x40 rune = commute_field1613._0
        var x41 int = commute_field1613._1
        var t1140 int = current__59 + x41
        ref_set__Ref_3int(index__58, t1140)
        var t1141 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t1142 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t1141,
        }
        return t1142
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
