package main

import (
    _goml_fmt "fmt"
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
        items: make([]string, 0, capacity),
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

type Tuple2_13Option__isize_13Option__isize struct {
    _0 Option__isize
    _1 Option__isize
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

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_isize_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_isize_c_char_q_
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

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

type _goml_m_Option_____o_isize_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

func main0() struct{} {
    var text__0 string = "  Héllo, World!  "
    var t432 string = _goml_m_inherent_i_string_i_string_i_trim(text__0)
    var t433 string = "[" + t432
    var t434 string = t433 + "]"
    println__T_string(t434)
    var t435 string = _goml_m_inherent_i_string_i_string_i_trim__start(text__0)
    var t436 string = "[" + t435
    var t437 string = t436 + "]"
    println__T_string(t437)
    var t438 string = _goml_m_inherent_i_string_i_string_i_trim__end(text__0)
    var t439 string = "[" + t438
    var t440 string = t439 + "]"
    println__T_string(t440)
    var t441 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(text__0, ",")
    var t442 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(t441, "|")
    println__T_string(t442)
    var t443 _goml_m_Option_____o_string_c_string_q_ = _goml_m_inherent_i_string_i_string_i_split__once(text__0, ",")
    var t444 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: "",
        _1: "",
    }
    var t445 Tuple2_6string_6string = _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(t443, t444)
    var t446 string = t445._1
    println__T_string(t446)
    var t447 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_lines(text__0)
    var t448 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t447)
    var t449 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t448)
    println__T_string(t449)
    var t450 Option__isize = _goml_m_inherent_i_string_i_string_i_find(text__0, "World")
    var t451 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t450, -1)
    var t452 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t451)
    println__T_string(t452)
    var t453 Option__isize = _goml_m_inherent_i_string_i_string_i_rfind(text__0, "l")
    var t454 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t453, -1)
    var t455 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t454)
    println__T_string(t455)
    var t456 Option__isize
    var inline1191 string = "lo"
    var inline1192 Option__isize = _goml_m_inherent_i_string_i_string_i_find(text__0, inline1191)
    t456 = inline1192
    var t457 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t456, -1)
    var t458 string
    var inline1189 string = _goml_runtime_core_int_to_string(t457)
    t458 = inline1189
    var inline1186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t458)
    _goml_runtime_core_string_println(inline1186)
    var t459 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(text__0, 2, "Hé")
    var t460 string
    var inline1184 string = _goml_runtime_core_bool_to_string(t459)
    t460 = inline1184
    var inline1181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t460)
    _goml_runtime_core_string_println(inline1181)
    var t461 int = _goml_m_inherent_i_string_i_string_i_char__count(text__0)
    var t462 string
    var inline1179 string = _goml_runtime_core_int_to_string(t461)
    t462 = inline1179
    var inline1176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t462)
    _goml_runtime_core_string_println(inline1176)
    var t463 Option__string = _goml_m_inherent_i_string_i_string_i_slice__chars(text__0, 2, 7)
    var t464 string
    var inline1172 string = "none"
    switch t463._tag {
    case 0:
        t464 = inline1172
    case 1:
        var inline1173 string = t463._v1_0
        t464 = inline1173
    default:
        panic("non-exhaustive match")
    }
    var inline1169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline1169)
    var t465 string = _goml_m_inherent_i_string_i_string_i_replace(text__0, "l", "L")
    var inline1166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline1166)
    var t466 string = _goml_m_inherent_i_string_i_string_i_repeat("ab", 3)
    var inline1163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t466)
    _goml_runtime_core_string_println(inline1163)
    var t467 bool = _goml_m_inherent_i_string_i_string_i_is__ascii(text__0)
    var t468 string
    var inline1161 string = _goml_runtime_core_bool_to_string(t467)
    t468 = inline1161
    var inline1158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline1158)
    var t469 bool = _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case("ABC", "abc")
    var t470 string
    var inline1156 string = _goml_runtime_core_bool_to_string(t469)
    t470 = inline1156
    var inline1153 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline1153)
    var t471 string = _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase("AbC")
    var inline1150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline1150)
    var t472 string = _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase("aBc")
    var inline1147 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
    _goml_runtime_core_string_println(inline1147)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t475 string
    t475 = value__1
    _goml_runtime_core_string_println(t475)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_trim(self__94 string) string {
    var t479 string = _goml_m_inherent_i_string_i_string_i_trim__start(self__94)
    var t480 string = _goml_m_inherent_i_string_i_string_i_trim__end(t479)
    return t480
}

func _goml_m_inherent_i_string_i_string_i_trim__start(self__90 string) string {
    var start__91 int = 0
    Loop_loop486:
    for {
        var t491 int
        var inline1197 int = _goml_runtime_core_string_len(self__90)
        t491 = inline1197
        var t492 bool = start__91 < t491
        var jp488 bool
        if t492 {
            var t493 uint8
            var inline1195 uint8 = _goml_runtime_core_string_byte_get(self__90, start__91)
            t493 = inline1195
            var t494 bool = ascii_is_whitespace(t493)
            jp488 = t494
        } else {
            jp488 = false
        }
        if jp488 {
            var compound_old77 int = start__91
            var compound_value78 int = 1
            var t489 int = compound_old77 + compound_value78
            start__91 = t489
            continue
        } else {
            break Loop_loop486
        }
    }
    var t484 int
    var inline1201 int = _goml_runtime_core_string_len(self__90)
    t484 = inline1201
    var inline1199 string = string_byte_slice(self__90, start__91, t484)
    return inline1199
}

func _goml_m_inherent_i_string_i_string_i_trim__end(self__92 string) string {
    var end__93 int
    var inline1208 int = _goml_runtime_core_string_len(self__92)
    end__93 = inline1208
    Loop_loop499:
    for {
        var t504 bool = end__93 > 0
        var jp501 bool
        if t504 {
            var t505 int = end__93 - 1
            var t506 uint8
            var inline1203 uint8 = _goml_runtime_core_string_byte_get(self__92, t505)
            t506 = inline1203
            var t507 bool = ascii_is_whitespace(t506)
            jp501 = t507
        } else {
            jp501 = false
        }
        if jp501 {
            var compound_old81 int = end__93
            var compound_value82 int = 1
            var t502 int = compound_old81 - compound_value82
            end__93 = t502
            continue
        } else {
            break Loop_loop499
        }
    }
    var inline1205 int = 0
    var inline1206 string = string_byte_slice(self__92, inline1205, end__93)
    return inline1206
}

func _goml_m_inherent_i_string_i_string_i_split(self__95 string, separator__96 string) *_goml_vec_string {
    var result__97 *_goml_vec_string
    var inline1224 *_goml_vec_string = vec_new__Vec_6string()
    result__97 = inline1224
    var separator_len__98 int
    var inline1222 int = _goml_runtime_core_string_len(separator__96)
    separator_len__98 = inline1222
    var value_len__99 int
    var inline1220 int = _goml_runtime_core_string_len(self__95)
    value_len__99 = inline1220
    var t517 bool = separator_len__98 == 0
    if t517 {
        vec_push__Vec_6string(result__97, self__95)
        return result__97
    } else {
        var start__100 int = 0
        Loop_loop_expr512:
        for {
            var mtmp87 Option__isize = string_find_from(self__95, separator__96, start__100)
            switch mtmp87._tag {
            case 0:
                var t514 string
                var inline1214 string = string_byte_slice(self__95, start__100, value_len__99)
                t514 = inline1214
                vec_push__Vec_6string(result__97, t514)
                break Loop_loop_expr512
            case 1:
                var x88 int = mtmp87._v1_0
                var t515 string
                var inline1218 string = string_byte_slice(self__95, start__100, x88)
                t515 = inline1218
                vec_push__Vec_6string(result__97, t515)
                var t516 int = x88 + separator_len__98
                start__100 = t516
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        return result__97
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t520 int
    var inline1263 int = vec_len__Vec_6string(self__378)
    t520 = inline1263
    var parts__380 *_goml_vec_string
    var inline1261 *_goml_vec_string = vec_with_capacity__Vec_6string(t520)
    parts__380 = inline1261
    var t521 int
    var inline1259 int = vec_len__Vec_6string(self__378)
    t521 = inline1259
    var t522 FnIterator__isize
    var inline1253 int = 0
    var inline1254 *ref_int_x = ref__Ref_3int(inline1253)
    var inline1255 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1254,
        end_1: t521,
    }
    var inline1256 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1255)
    }
    var inline1257 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1256)
    t522 = inline1257
    var for_iter349 FnIterator__isize
    for_iter349 = t522
    Loop_loop537:
    for {
        var for_next350 Option__isize
        var inline1229 func() Option__isize = for_iter349.next_fn
        var inline1230 Option__isize = inline1229()
        for_next350 = inline1230
        switch for_next350._tag {
        case 0:
            break Loop_loop537
        case 1:
            var x351 int = for_next350._v1_0
            var t539 string = vec_get__Vec_6string(self__378, x351)
            var t540 string
            t540 = t539
            vec_push__Vec_6string(parts__380, t540)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t524 int
    var inline1250 int = vec_len__Vec_6string(parts__380)
    t524 = inline1250
    var t525 int = t524 * 2
    var result__382 *_goml_vec_string
    var inline1248 *_goml_vec_string = vec_with_capacity__Vec_6string(t525)
    result__382 = inline1248
    var t526 int
    var inline1246 int = vec_len__Vec_6string(parts__380)
    t526 = inline1246
    var t527 FnIterator__isize
    var inline1240 int = 0
    var inline1241 *ref_int_x = ref__Ref_3int(inline1240)
    var inline1242 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1241,
        end_1: t526,
    }
    var inline1243 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1242)
    }
    var inline1244 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1243)
    t527 = inline1244
    var for_iter353 FnIterator__isize
    for_iter353 = t527
    Loop_loop530:
    for {
        var for_next354 Option__isize
        var inline1236 func() Option__isize = for_iter353.next_fn
        var inline1237 Option__isize = inline1236()
        for_next354 = inline1237
        switch for_next354._tag {
        case 0:
            break Loop_loop530
        case 1:
            var x355 int = for_next354._v1_0
            var t535 bool = x355 > 0
            if t535 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t533 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t533)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t529 string = _goml_runtime_core_string_concat(result__382)
    return t529
}

func _goml_m_inherent_i_string_i_string_i_split__once(self__102 string, separator__103 string) _goml_m_Option_____o_string_c_string_q_ {
    var separator_len__104 int
    var inline1274 int = _goml_runtime_core_string_len(separator__103)
    separator_len__104 = inline1274
    var value_len__105 int
    var inline1272 int = _goml_runtime_core_string_len(self__102)
    value_len__105 = inline1272
    var t546 bool = separator_len__104 == 0
    if t546 {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    } else {
        var mtmp93 Option__isize
        var inline1270 Option__isize = string_find_from(self__102, separator__103, 0)
        mtmp93 = inline1270
        switch mtmp93._tag {
        case 0:
            return _goml_m_Option_____o_string_c_string_q_{
                _tag: 0,
            }
        case 1:
            var x94 int = mtmp93._v1_0
            var t549 string
            var inline1267 int = 0
            var inline1268 string = string_byte_slice(self__102, inline1267, x94)
            t549 = inline1268
            var t550 int = x94 + separator_len__104
            var t551 string
            var inline1265 string = string_byte_slice(self__102, t550, value_len__105)
            t551 = inline1265
            var t552 Tuple2_6string_6string = Tuple2_6string_6string{
                _0: t549,
                _1: t551,
            }
            var t553 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
                _tag: 1,
                _v1_0: t552,
            }
            return t553
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(self__467 _goml_m_Option_____o_string_c_string_q_, fallback__468 Tuple2_6string_6string) Tuple2_6string_6string {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 Tuple2_6string_6string = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_string_i_string_i_lines(self__107 string) *_goml_vec_string {
    var result__108 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(self__107, "\n")
    var t580 bool
    var inline1309 int = vec_len__Vec_6string(result__108)
    var inline1310 bool = inline1309 == 0
    t580 = inline1310
    var t581 bool = !t580
    var jp579 bool
    if t581 {
        var t582 int
        var inline1276 int = vec_len__Vec_6string(result__108)
        t582 = inline1276
        var t583 int = t582 - 1
        var t584 string = vec_get__Vec_6string(result__108, t583)
        var t585 bool = t584 == ""
        jp579 = t585
    } else {
        jp579 = false
    }
    if jp579 {
        var inline1278 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(result__108)
        var inline1279 bool = inline1278 == 0
        if inline1279 {} else {
            var inline1280 int = inline1278 - 1
            vec_get__Vec_6string(result__108, inline1280)
            var inline1282 int = inline1278 - 1
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(result__108, inline1282)
        }
    } else {}
    var t561 int
    var inline1307 int = vec_len__Vec_6string(result__108)
    t561 = inline1307
    var t562 FnIterator__isize
    var inline1301 int = 0
    var inline1302 *ref_int_x = ref__Ref_3int(inline1301)
    var inline1303 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1302,
        end_1: t561,
    }
    var inline1304 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1303)
    }
    var inline1305 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1304)
    t562 = inline1305
    var for_iter97 FnIterator__isize
    for_iter97 = t562
    Loop_loop564:
    for {
        var for_next98 Option__isize
        var inline1297 func() Option__isize = for_iter97.next_fn
        var inline1298 Option__isize = inline1297()
        for_next98 = inline1298
        switch for_next98._tag {
        case 0:
            break Loop_loop564
        case 1:
            var x99 int = for_next98._v1_0
            var line__110 string = vec_get__Vec_6string(result__108, x99)
            var t572 int
            var inline1295 int = _goml_runtime_core_string_len(line__110)
            t572 = inline1295
            var t573 bool = t572 > 0
            var jp568 bool
            if t573 {
                var t574 int
                var inline1288 int = _goml_runtime_core_string_len(line__110)
                t574 = inline1288
                var t575 int = t574 - 1
                var t576 uint8
                var inline1286 uint8 = _goml_runtime_core_string_byte_get(line__110, t575)
                t576 = inline1286
                var t577 bool = t576 == 13
                jp568 = t577
            } else {
                jp568 = false
            }
            if jp568 {
                vec_get__Vec_6string(result__108, x99)
                var t569 int
                var inline1293 int = _goml_runtime_core_string_len(line__110)
                t569 = inline1293
                var t570 int = t569 - 1
                var value103 string
                var inline1290 int = 0
                var inline1291 string = string_byte_slice(line__110, inline1290, t570)
                value103 = inline1291
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
    var t588 int = vec_len__Vec_6string(self__273)
    return t588
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t591 string = _goml_runtime_core_int_to_string(self__32)
    return t591
}

func _goml_m_inherent_i_string_i_string_i_find(self__69 string, expected__70 string) Option__isize {
    var t594 Option__isize = string_find_from(self__69, expected__70, 0)
    return t594
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(self__467 Option__isize, fallback__468 int) int {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 int = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_string_i_string_i_rfind(self__85 string, expected__86 string) Option__isize {
    var value_len__87 int
    var inline1314 int = _goml_runtime_core_string_len(self__85)
    value_len__87 = inline1314
    var expected_len__88 int
    var inline1312 int = _goml_runtime_core_string_len(expected__86)
    expected_len__88 = inline1312
    var t603 bool = expected_len__88 > value_len__87
    if t603 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var start__89 int = value_len__87 - expected_len__88
        Loop_loop605:
        for {
            var t606 bool = start__89 >= 0
            if t606 {
                var t608 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(self__85, start__89, expected__86)
                if t608 {
                    var t609 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: start__89,
                    }
                    return t609
                } else {
                    var compound_old73 int = start__89
                    var compound_value74 int = 1
                    var t610 int = compound_old73 - compound_value74
                    start__89 = t610
                    continue
                }
            } else {
                break Loop_loop605
            }
        }
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_starts__with__at(self__63 string, start__64 int, prefix__65 string) bool {
    var value_len__66 int
    var inline1326 int = _goml_runtime_core_string_len(self__63)
    value_len__66 = inline1326
    var prefix_len__67 int
    var inline1324 int = _goml_runtime_core_string_len(prefix__65)
    prefix_len__67 = inline1324
    var t633 bool = start__64 < 0
    var jp630 bool
    if t633 {
        jp630 = true
    } else {
        var t634 bool = start__64 > value_len__66
        jp630 = t634
    }
    var jp620 bool
    if jp630 {
        jp620 = true
    } else {
        var t631 int = value_len__66 - start__64
        var t632 bool = prefix_len__67 > t631
        jp620 = t632
    }
    if jp620 {
        return false
    } else {
        var end__68 int = start__64 + prefix_len__67
        var t627 bool
        var inline1322 bool = string_is_char_boundary(self__63, start__64)
        t627 = inline1322
        var jp624 bool
        if t627 {
            var inline1318 bool = string_is_char_boundary(self__63, end__68)
            jp624 = inline1318
        } else {
            jp624 = false
        }
        if jp624 {
            var t625 string
            var inline1320 string = string_byte_slice(self__63, start__64, end__68)
            t625 = inline1320
            var t626 bool = t625 == prefix__65
            return t626
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_char__count(self__73 string) int {
    var count__74 int = 0
    var t640 FnIterator__char
    var inline1332 *ref_int_x = ref__Ref_3int(0)
    var inline1333 closure_env_inherent_string_string_chars_1 = closure_env_inherent_string_string_chars_1{
        self_0: self__73,
        index_1: inline1332,
    }
    var inline1334 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(inline1333)
    }
    var inline1335 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1334)
    t640 = inline1335
    var for_iter43 FnIterator__char
    for_iter43 = t640
    Loop_loop642:
    for {
        var for_next44 Option__char
        var inline1328 func() Option__char = for_iter43.next_fn
        var inline1329 Option__char = inline1328()
        for_next44 = inline1329
        switch for_next44._tag {
        case 0:
            break Loop_loop642
        case 1:
            var compound_old46 int = count__74
            var compound_value47 int = 1
            var t644 int = compound_old46 + compound_value47
            count__74 = t644
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return count__74
}

func _goml_m_inherent_i_string_i_string_i_slice__chars(self__75 string, start__76 int, end__77 int) Option__string {
    var t686 bool = start__76 < 0
    var jp651 bool
    if t686 {
        jp651 = true
    } else {
        var t687 bool = end__77 < start__76
        jp651 = t687
    }
    if jp651 {
        return Option__string{
            _tag: 0,
        }
    } else {
        var char_index__78 int = 0
        var t684 bool = start__76 == 0
        var jp653 Option__isize
        if t684 {
            var t685 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: 0,
            }
            jp653 = t685
        } else {
            jp653 = Option__isize{
                _tag: 0,
            }
        }
        var start_byte__79 Option__isize = jp653
        var t682 bool = end__77 == 0
        var jp655 Option__isize
        if t682 {
            var t683 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: 0,
            }
            jp655 = t683
        } else {
            jp655 = Option__isize{
                _tag: 0,
            }
        }
        var end_byte__80 Option__isize = jp655
        var t656 _goml_m_FnIterator_____o_isize_c_char_q_
        var inline1347 *ref_int_x = ref__Ref_3int(0)
        var inline1348 closure_env_inherent_string_string_char_indices_2 = closure_env_inherent_string_string_char_indices_2{
            index_0: inline1347,
            self_1: self__75,
        }
        var inline1349 func() _goml_m_Option_____o_isize_c_char_q_ = func() _goml_m_Option_____o_isize_c_char_q_ {
            return _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(inline1348)
        }
        var inline1350 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(inline1349)
        t656 = inline1350
        var for_iter50 _goml_m_FnIterator_____o_isize_c_char_q_
        for_iter50 = t656
        Loop_loop672:
        for {
            var for_next51 _goml_m_Option_____o_isize_c_char_q_
            var inline1337 func() _goml_m_Option_____o_isize_c_char_q_ = for_iter50.next_fn
            var inline1338 _goml_m_Option_____o_isize_c_char_q_ = inline1337()
            for_next51 = inline1338
            switch for_next51._tag {
            case 0:
                break Loop_loop672
            case 1:
                var x52 Tuple2_3int_4char = for_next51._v1_0
                var x54 int = x52._0
                var t680 bool = char_index__78 == start__76
                if t680 {
                    var t681 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x54,
                    }
                    start_byte__79 = t681
                } else {}
                var t678 bool = char_index__78 == end__77
                if t678 {
                    var t679 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x54,
                    }
                    end_byte__80 = t679
                } else {}
                var compound_old60 int = char_index__78
                var compound_value61 int = 1
                var t676 int = compound_old60 + compound_value61
                char_index__78 = t676
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t669 bool = char_index__78 == start__76
        if t669 {
            var t670 int
            var inline1340 int = _goml_runtime_core_string_len(self__75)
            t670 = inline1340
            var t671 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: t670,
            }
            start_byte__79 = t671
        } else {}
        var t666 bool = char_index__78 == end__77
        if t666 {
            var t667 int
            var inline1342 int = _goml_runtime_core_string_len(self__75)
            t667 = inline1342
            var t668 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: t667,
            }
            end_byte__80 = t668
        } else {}
        var mtmp68 Tuple2_13Option__isize_13Option__isize = Tuple2_13Option__isize_13Option__isize{
            _0: start_byte__79,
            _1: end_byte__80,
        }
        var x69 Option__isize = mtmp68._0
        var x70 Option__isize = mtmp68._1
        switch x70._tag {
        case 1:
            var x71 int = x70._v1_0
            switch x69._tag {
            case 1:
                var x72 int = x69._v1_0
                var t664 string
                var inline1344 string = string_byte_slice(self__75, x72, x71)
                t664 = inline1344
                var t665 Option__string = Option__string{
                    _tag: 1,
                    _v1_0: t664,
                }
                return t665
            default:
                return Option__string{
                    _tag: 0,
                }
            }
        default:
            return Option__string{
                _tag: 0,
            }
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_replace(self__111 string, expected__112 string, replacement__113 string) string {
    var t696 int
    var inline1368 int = _goml_runtime_core_string_len(expected__112)
    t696 = inline1368
    var t697 bool = t696 == 0
    if t697 {
        return self__111
    } else {
        var parts__114 *_goml_vec_string
        var inline1366 *_goml_vec_string = vec_new__Vec_6string()
        parts__114 = inline1366
        var start__115 int = 0
        Loop_loop_expr700:
        for {
            var mtmp106 Option__isize = string_find_from(self__111, expected__112, start__115)
            switch mtmp106._tag {
            case 0:
                var t702 int
                var inline1356 int = _goml_runtime_core_string_len(self__111)
                t702 = inline1356
                var t703 string
                var inline1354 string = string_byte_slice(self__111, start__115, t702)
                t703 = inline1354
                vec_push__Vec_6string(parts__114, t703)
                break Loop_loop_expr700
            case 1:
                var x107 int = mtmp106._v1_0
                var t704 string
                var inline1364 string = string_byte_slice(self__111, start__115, x107)
                t704 = inline1364
                vec_push__Vec_6string(parts__114, t704)
                vec_push__Vec_6string(parts__114, replacement__113)
                var t705 int
                var inline1358 int = _goml_runtime_core_string_len(expected__112)
                t705 = inline1358
                var t706 int = x107 + t705
                start__115 = t706
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t699 string = _goml_runtime_core_string_concat(parts__114)
        return t699
    }
}

func _goml_m_inherent_i_string_i_string_i_repeat(self__117 string, count__118 int) string {
    var t719 bool = count__118 <= 0
    var jp712 bool
    if t719 {
        jp712 = true
    } else {
        var t720 int
        var inline1370 int = _goml_runtime_core_string_len(self__117)
        t720 = inline1370
        var t721 bool = t720 == 0
        jp712 = t721
    }
    if jp712 {
        return ""
    } else {
        var parts__119 *_goml_vec_string
        var inline1384 *_goml_vec_string = vec_with_capacity__Vec_6string(count__118)
        parts__119 = inline1384
        var t713 FnIterator__isize
        var inline1378 int = 0
        var inline1379 *ref_int_x = ref__Ref_3int(inline1378)
        var inline1380 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1379,
            end_1: count__118,
        }
        var inline1381 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1380)
        }
        var inline1382 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1381)
        t713 = inline1382
        var for_iter113 FnIterator__isize
        for_iter113 = t713
        Loop_loop716:
        for {
            var for_next114 Option__isize
            var inline1374 func() Option__isize = for_iter113.next_fn
            var inline1375 Option__isize = inline1374()
            for_next114 = inline1375
            switch for_next114._tag {
            case 0:
                break Loop_loop716
            case 1:
                vec_push__Vec_6string(parts__119, self__117)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t715 string = _goml_runtime_core_string_concat(parts__119)
        return t715
    }
}

func _goml_m_inherent_i_string_i_string_i_is__ascii(self__120 string) bool {
    var t724 int
    var inline1398 int = _goml_runtime_core_string_len(self__120)
    t724 = inline1398
    var t725 FnIterator__isize
    var inline1392 int = 0
    var inline1393 *ref_int_x = ref__Ref_3int(inline1392)
    var inline1394 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1393,
        end_1: t724,
    }
    var inline1395 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1394)
    }
    var inline1396 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1395)
    t725 = inline1396
    var for_iter117 FnIterator__isize
    for_iter117 = t725
    Loop_loop727:
    for {
        var for_next118 Option__isize
        var inline1388 func() Option__isize = for_iter117.next_fn
        var inline1389 Option__isize = inline1388()
        for_next118 = inline1389
        switch for_next118._tag {
        case 0:
            break Loop_loop727
        case 1:
            var x119 int = for_next118._v1_0
            var t730 uint8
            var inline1386 uint8 = _goml_runtime_core_string_byte_get(self__120, x119)
            t730 = inline1386
            var t731 bool = t730 > 127
            if t731 {
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
    var t736 int
    var inline1422 int = _goml_runtime_core_string_len(self__122)
    t736 = inline1422
    var t737 int
    var inline1420 int = _goml_runtime_core_string_len(other__123)
    t737 = inline1420
    var t738 bool = t736 != t737
    if t738 {
        return false
    } else {
        var t739 int
        var inline1418 int = _goml_runtime_core_string_len(self__122)
        t739 = inline1418
        var t740 FnIterator__isize
        var inline1412 int = 0
        var inline1413 *ref_int_x = ref__Ref_3int(inline1412)
        var inline1414 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1413,
            end_1: t739,
        }
        var inline1415 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1414)
        }
        var inline1416 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1415)
        t740 = inline1416
        var for_iter121 FnIterator__isize
        for_iter121 = t740
        Loop_loop742:
        for {
            var for_next122 Option__isize
            var inline1408 func() Option__isize = for_iter121.next_fn
            var inline1409 Option__isize = inline1408()
            for_next122 = inline1409
            switch for_next122._tag {
            case 0:
                break Loop_loop742
            case 1:
                var x123 int = for_next122._v1_0
                var t745 uint8
                var inline1406 uint8 = _goml_runtime_core_string_byte_get(self__122, x123)
                t745 = inline1406
                var t746 uint8
                var inline1404 uint8 = _goml_runtime_core_string_byte_get(other__123, x123)
                t746 = inline1404
                var t747 bool
                var inline1400 uint8 = ascii_to_lowercase(t745)
                var inline1401 uint8 = ascii_to_lowercase(t746)
                var inline1402 bool = inline1400 == inline1401
                t747 = inline1402
                var t748 bool = !t747
                if t748 {
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
    var inline1440 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__125)
    values__126 = inline1440
    var t751 int
    var inline1438 int = vec_len__Vec_5uint8(values__126)
    t751 = inline1438
    var t752 FnIterator__isize
    var inline1432 int = 0
    var inline1433 *ref_int_x = ref__Ref_3int(inline1432)
    var inline1434 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1433,
        end_1: t751,
    }
    var inline1435 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1434)
    }
    var inline1436 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1435)
    t752 = inline1436
    var for_iter125 FnIterator__isize
    for_iter125 = t752
    var inline1425 uint8 = 97 - 65
    Loop_loop754:
    for {
        var for_next126 Option__isize
        var inline1428 func() Option__isize = for_iter125.next_fn
        var inline1429 Option__isize = inline1428()
        for_next126 = inline1429
        switch for_next126._tag {
        case 0:
            break Loop_loop754
        case 1:
            var x127 int = for_next126._v1_0
            vec_get__Vec_5uint8(values__126, x127)
            var t756 uint8 = vec_get__Vec_5uint8(values__126, x127)
            var value131 uint8
            var inline1424 bool = ascii_is_uppercase(t756)
            if inline1424 {
                var inline1426 uint8 = t756 + inline1425
                value131 = inline1426
            } else {
                value131 = t756
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
    var inline1458 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__129)
    values__130 = inline1458
    var t760 int
    var inline1456 int = vec_len__Vec_5uint8(values__130)
    t760 = inline1456
    var t761 FnIterator__isize
    var inline1450 int = 0
    var inline1451 *ref_int_x = ref__Ref_3int(inline1450)
    var inline1452 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1451,
        end_1: t760,
    }
    var inline1453 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1452)
    }
    var inline1454 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1453)
    t761 = inline1454
    var for_iter137 FnIterator__isize
    for_iter137 = t761
    var inline1443 uint8 = 97 - 65
    Loop_loop763:
    for {
        var for_next138 Option__isize
        var inline1446 func() Option__isize = for_iter137.next_fn
        var inline1447 Option__isize = inline1446()
        for_next138 = inline1447
        switch for_next138._tag {
        case 0:
            break Loop_loop763
        case 1:
            var x139 int = for_next138._v1_0
            vec_get__Vec_5uint8(values__130, x139)
            var t765 uint8 = vec_get__Vec_5uint8(values__130, x139)
            var value143 uint8
            var inline1442 bool = ascii_is_lowercase(t765)
            if inline1442 {
                var inline1444 uint8 = t765 - inline1443
                value143 = inline1444
            } else {
                value143 = t765
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
    var t771 int = _goml_runtime_core_string_len(self__36)
    return t771
}

func ascii_is_whitespace(value__140 uint8) bool {
    var t788 bool = value__140 == 9
    var jp786 bool
    if t788 {
        jp786 = true
    } else {
        var t789 bool = value__140 == 10
        jp786 = t789
    }
    var jp783 bool
    if jp786 {
        jp783 = true
    } else {
        var t787 bool = value__140 == 11
        jp783 = t787
    }
    var jp780 bool
    if jp783 {
        jp780 = true
    } else {
        var t784 bool = value__140 == 12
        jp780 = t784
    }
    var jp777 bool
    if jp780 {
        jp777 = true
    } else {
        var t781 bool = value__140 == 13
        jp777 = t781
    }
    if jp777 {
        return true
    } else {
        var t778 bool = value__140 == 32
        return t778
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t792 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t792
}

func string_find_from(value__133 string, expected__134 string, start__135 int) Option__isize {
    var value_len__136 int
    var inline1469 int = _goml_runtime_core_string_len(value__133)
    value_len__136 = inline1469
    var expected_len__137 int
    var inline1467 int = _goml_runtime_core_string_len(expected__134)
    expected_len__137 = inline1467
    var t823 bool = start__135 < 0
    var jp806 bool
    if t823 {
        jp806 = true
    } else {
        var t824 bool = start__135 > value_len__136
        jp806 = t824
    }
    if jp806 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var t809 bool = expected_len__137 == 0
        if t809 {
            var t810 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: start__135,
            }
            return t810
        } else {
            var t813 int = value_len__136 - start__135
            var t814 bool = expected_len__137 > t813
            if t814 {
                return Option__isize{
                    _tag: 0,
                }
            } else {
                var limit__138 int = value_len__136 - expected_len__137
                var index__139 int = start__135
                Loop_loop816:
                for {
                    var t817 bool = index__139 <= limit__138
                    if t817 {
                        var t819 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(value__133, index__139, expected__134)
                        if t819 {
                            var t820 Option__isize = Option__isize{
                                _tag: 1,
                                _v1_0: index__139,
                            }
                            return t820
                        } else {
                            var compound_old149 int = index__139
                            var compound_value150 int = 1
                            var t821 int = compound_old149 + compound_value150
                            index__139 = t821
                            continue
                        }
                    } else {
                        break Loop_loop816
                    }
                }
                return Option__isize{
                    _tag: 0,
                }
            }
        }
    }
}

func ascii_to_lowercase(value__143 uint8) uint8 {
    var t891 bool
    var inline1489 bool = value__143 >= 65
    if inline1489 {
        var inline1490 bool = value__143 <= 90
        t891 = inline1490
    } else {
        t891 = false
    }
    if t891 {
        var t892 uint8 = 97 - 65
        var t893 uint8 = value__143 + t892
        return t893
    } else {
        return value__143
    }
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop898:
    for {
        var t899 int
        var inline1492 int = _goml_runtime_core_string_len(x12)
        t899 = inline1492
        var t900 bool = index__26 < t899
        if t900 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t902 int = compound_old17 + x16
                index__26 = t902
                continue
            } else {
                var t904 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t904
            }
        } else {
            break Loop_loop898
        }
    }
    var t897 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t897
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t920 bool = string_is_char_boundary(value__21, start__22)
    var jp917 bool
    if t920 {
        var t921 bool = string_is_char_boundary(value__21, end__23)
        jp917 = t921
    } else {
        jp917 = false
    }
    if jp917 {
        var t918 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t918
    } else {
        var t919 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t919
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__254 func() Option__isize) FnIterator__isize {
    var t924 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__254,
    }
    return t924
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(self__278 *_goml_vec_string, len__279 int) struct{} {
    vec_truncate__Vec_6string(self__278, len__279)
    return struct{}{}
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t940 bool = index__16 < 0
    var jp932 bool
    if t940 {
        jp932 = true
    } else {
        var t941 int
        var inline1497 int = _goml_runtime_core_string_len(value__15)
        t941 = inline1497
        var t942 bool = index__16 > t941
        jp932 = t942
    }
    if jp932 {
        return false
    } else {
        var t935 int
        var inline1501 int = _goml_runtime_core_string_len(value__15)
        t935 = inline1501
        var t936 bool = index__16 == t935
        if t936 {
            return true
        } else {
            var t937 uint8
            var inline1499 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t937 = inline1499
            var t938_rhs uint8 = 192
            var t938 uint8 = t937 & t938_rhs
            var t939 bool = t938 != 128
            return t939
        }
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__254 func() Option__char) FnIterator__char {
    var t951 FnIterator__char = FnIterator__char{
        next_fn: next_fn__254,
    }
    return t951
}

func _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(next_fn__254 func() _goml_m_Option_____o_isize_c_char_q_) _goml_m_FnIterator_____o_isize_c_char_q_ {
    var t954 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_FnIterator_____o_isize_c_char_q_{
        next_fn: next_fn__254,
    }
    return t954
}

func ascii_is_uppercase(value__141 uint8) bool {
    var t959 bool = value__141 >= 65
    if t959 {
        var t960 bool = value__141 <= 90
        return t960
    } else {
        return false
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1079 bool = index__6 < 0
    var jp1077 bool
    if t1079 {
        jp1077 = true
    } else {
        var t1080 bool = index__6 >= length__7
        jp1077 = t1080
    }
    if jp1077 {
        var inline1503 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1503
    } else {
        var t964 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t964))
        var t967 bool = first__8 < 128
        if t967 {
            var inline1505 int = 1
            var inline1506 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1506._tag {
            case 0:
                var inline1507 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1507
            case 1:
                var inline1508 rune = inline1506._v1_0
                var inline1510 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1508,
                    _2: inline1505,
                }
                return inline1510
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t971 bool = first__8 < 194
            if t971 {
                var inline1512 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1512
            } else {
                var t975 bool = first__8 < 224
                if t975 {
                    var t988 int = length__7 - index__6
                    var t989 bool = t988 < 2
                    if t989 {
                        var inline1514 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1514
                    } else {
                        var t977 int = index__6 + 1
                        var t978 uint8
                        var inline1528 uint8 = _goml_runtime_core_string_byte_get(value__5, t977)
                        t978 = inline1528
                        var second__9 uint32 = uint32(uint8(t978))
                        var t981 bool
                        var inline1525 bool = second__9 < 128
                        if inline1525 {
                            t981 = true
                        } else {
                            var inline1526 bool = second__9 > 191
                            t981 = inline1526
                        }
                        if t981 {
                            var inline1516 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1516
                        } else {
                            var t983_rhs uint32 = 31
                            var t983 uint32 = first__8 & t983_rhs
                            var t984_rhs int = 6
                            var t984 uint32 = t983 << t984_rhs
                            var t985_rhs uint32 = 63
                            var t985 uint32 = second__9 & t985_rhs
                            var t986 uint32 = t984 | t985
                            var inline1518 int = 2
                            var inline1519 Option__char = __goml_builtin_char_from_uint32(t986)
                            switch inline1519._tag {
                            case 0:
                                var inline1520 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1520
                            case 1:
                                var inline1521 rune = inline1519._v1_0
                                var inline1523 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1521,
                                    _2: inline1518,
                                }
                                return inline1523
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t993 bool = first__8 < 240
                    if t993 {
                        var t1026 int = length__7 - index__6
                        var t1027 bool = t1026 < 3
                        if t1027 {
                            var inline1530 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1530
                        } else {
                            var t995 int = index__6 + 1
                            var t996 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t995)
                            var second__10 uint32 = uint32(uint8(t996))
                            var t997 int = index__6 + 2
                            var t998 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t997)
                            var third__11 uint32 = uint32(uint8(t998))
                            var t1024 bool = utf8_invalid_continuation(second__10)
                            var jp1019 bool
                            if t1024 {
                                jp1019 = true
                            } else {
                                var inline1532 bool = third__11 < 128
                                if inline1532 {
                                    jp1019 = true
                                } else {
                                    var inline1533 bool = third__11 > 191
                                    jp1019 = inline1533
                                }
                            }
                            var jp1013 bool
                            if jp1019 {
                                jp1013 = true
                            } else {
                                var t1022 bool = first__8 == 224
                                if t1022 {
                                    var t1023 bool = second__10 < 160
                                    jp1013 = t1023
                                } else {
                                    jp1013 = false
                                }
                            }
                            var jp1002 bool
                            if jp1013 {
                                jp1002 = true
                            } else {
                                var t1016 bool = first__8 == 237
                                if t1016 {
                                    var t1017 bool = second__10 >= 160
                                    jp1002 = t1017
                                } else {
                                    jp1002 = false
                                }
                            }
                            if jp1002 {
                                var inline1535 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1535
                            } else {
                                var t1004_rhs uint32 = 15
                                var t1004 uint32 = first__8 & t1004_rhs
                                var t1005_rhs int = 12
                                var t1005 uint32 = t1004 << t1005_rhs
                                var t1006_rhs uint32 = 63
                                var t1006 uint32 = second__10 & t1006_rhs
                                var t1007_rhs int = 6
                                var t1007 uint32 = t1006 << t1007_rhs
                                var t1008 uint32 = t1005 | t1007
                                var t1009_rhs uint32 = 63
                                var t1009 uint32 = third__11 & t1009_rhs
                                var t1010 uint32 = t1008 | t1009
                                var inline1537 int = 3
                                var inline1538 Option__char = __goml_builtin_char_from_uint32(t1010)
                                switch inline1538._tag {
                                case 0:
                                    var inline1539 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1539
                                case 1:
                                    var inline1540 rune = inline1538._v1_0
                                    var inline1542 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1540,
                                        _2: inline1537,
                                    }
                                    return inline1542
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1031 bool = first__8 < 245
                        if t1031 {
                            var t1072 int = length__7 - index__6
                            var t1073 bool = t1072 < 4
                            if t1073 {
                                var t1074 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1074
                            } else {
                                var t1033 int = index__6 + 1
                                var t1034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1033)
                                var second__12 uint32 = uint32(uint8(t1034))
                                var t1035 int = index__6 + 2
                                var t1036 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1035)
                                var third__13 uint32 = uint32(uint8(t1036))
                                var t1037 int = index__6 + 3
                                var t1038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1037)
                                var fourth__14 uint32 = uint32(uint8(t1038))
                                var t1070 bool = utf8_invalid_continuation(second__12)
                                var jp1068 bool
                                if t1070 {
                                    jp1068 = true
                                } else {
                                    var t1071 bool = utf8_invalid_continuation(third__13)
                                    jp1068 = t1071
                                }
                                var jp1062 bool
                                if jp1068 {
                                    jp1062 = true
                                } else {
                                    var t1069 bool = utf8_invalid_continuation(fourth__14)
                                    jp1062 = t1069
                                }
                                var jp1056 bool
                                if jp1062 {
                                    jp1056 = true
                                } else {
                                    var t1065 bool = first__8 == 240
                                    if t1065 {
                                        var t1066 bool = second__12 < 144
                                        jp1056 = t1066
                                    } else {
                                        jp1056 = false
                                    }
                                }
                                var jp1042 bool
                                if jp1056 {
                                    jp1042 = true
                                } else {
                                    var t1059 bool = first__8 == 244
                                    if t1059 {
                                        var t1060 bool = second__12 > 143
                                        jp1042 = t1060
                                    } else {
                                        jp1042 = false
                                    }
                                }
                                if jp1042 {
                                    var t1043 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1043
                                } else {
                                    var t1044_rhs uint32 = 7
                                    var t1044 uint32 = first__8 & t1044_rhs
                                    var t1045_rhs int = 18
                                    var t1045 uint32 = t1044 << t1045_rhs
                                    var t1046_rhs uint32 = 63
                                    var t1046 uint32 = second__12 & t1046_rhs
                                    var t1047_rhs int = 12
                                    var t1047 uint32 = t1046 << t1047_rhs
                                    var t1048 uint32 = t1045 | t1047
                                    var t1049_rhs uint32 = 63
                                    var t1049 uint32 = third__13 & t1049_rhs
                                    var t1050_rhs int = 6
                                    var t1050 uint32 = t1049 << t1050_rhs
                                    var t1051 uint32 = t1048 | t1050
                                    var t1052_rhs uint32 = 63
                                    var t1052 uint32 = fourth__14 & t1052_rhs
                                    var t1053 uint32 = t1051 | t1052
                                    var t1054 Tuple3_4bool_4char_3int = utf8_valid_decode(t1053, 4)
                                    return t1054
                                }
                            }
                        } else {
                            var t1075 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1075
                        }
                    }
                }
            }
        }
    }
}

func ascii_is_lowercase(value__142 uint8) bool {
    var t1085 bool = value__142 >= 97
    if t1085 {
        var t1086 bool = value__142 <= 122
        return t1086
    } else {
        return false
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1089 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1089
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1610 rune
    var inline1546 bool = utf8_valid_scalar(value__0)
    if inline1546 {
        var inline1547 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1548 rune = inline1547._1
        commute_field1610 = inline1548
        var t1095 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1610,
            _2: width__1,
        }
        return t1095
    } else {
        var inline1544 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1544
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1100 bool = value__3 < 128
    if t1100 {
        return true
    } else {
        var t1101 bool = value__3 > 191
        return t1101
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1106 bool
    var inline1552 bool = value__30 <= 1114111
    if inline1552 {
        var inline1553 bool = value__30 >= 55296
        var inline1555 bool
        if inline1553 {
            var inline1557 bool = value__30 <= 57343
            inline1555 = inline1557
        } else {
            inline1555 = false
        }
        var inline1556 bool = !inline1555
        t1106 = inline1556
    } else {
        t1106 = false
    }
    if t1106 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1107 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1107
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1112 bool = value__4 <= 1114111
    if t1112 {
        var t1116 bool = value__4 >= 55296
        var jp1114 bool
        if t1116 {
            var t1117 bool = value__4 <= 57343
            jp1114 = t1117
        } else {
            jp1114 = false
        }
        var t1115 bool = !jp1114
        return t1115
    } else {
        return false
    }
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env428 closure_env_goml_builtin_range_0) Option__isize {
    var current__505 *ref_int_x = env428.current_0
    var end__504 int = env428.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t1128 bool = value__506 < end__504
    if t1128 {
        var t1129 int = value__506 + 1
        ref_set__Ref_3int(current__505, t1129)
        var t1130 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__506,
        }
        return t1130
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(env429 closure_env_inherent_string_string_chars_1) Option__char {
    var self__52 string = env429.self_0
    var index__53 *ref_int_x = env429.index_1
    var t1133 int = ref_get__Ref_3int(index__53)
    var commute_field1613 Tuple2_4char_3int
    var inline1559 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t1133)
    var inline1560 bool = inline1559._0
    var inline1561 rune = inline1559._1
    var inline1562 int = inline1559._2
    if inline1560 {
        var inline1566 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1561,
            _1: inline1562,
        }
        commute_field1613 = inline1566
        var x32 rune = commute_field1613._0
        var x33 int = commute_field1613._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t1136 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t1136)
        var t1138 Option__char = Option__char{
            _tag: 1,
            _v1_0: x32,
        }
        return t1138
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(env430 closure_env_inherent_string_string_char_indices_2) _goml_m_Option_____o_isize_c_char_q_ {
    var index__58 *ref_int_x = env430.index_0
    var self__57 string = env430.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1616 Tuple2_4char_3int
    var inline1569 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1570 bool = inline1569._0
    var inline1571 rune = inline1569._1
    var inline1572 int = inline1569._2
    if inline1570 {
        var inline1576 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1571,
            _1: inline1572,
        }
        commute_field1616 = inline1576
        var x40 rune = commute_field1616._0
        var x41 int = commute_field1616._1
        var t1143 int = current__59 + x41
        ref_set__Ref_3int(index__58, t1143)
        var t1144 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t1145 _goml_m_Option_____o_isize_c_char_q_ = _goml_m_Option_____o_isize_c_char_q_{
            _tag: 1,
            _v1_0: t1144,
        }
        return t1145
    } else {
        return _goml_m_Option_____o_isize_c_char_q_{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
