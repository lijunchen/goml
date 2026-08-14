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
    var inline1187 string = "lo"
    var inline1188 Option__int = _goml_m_inherent_i_string_i_string_i_find(text__0, inline1187)
    t453 = inline1188
    var t454 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t453, -1)
    var t455 string
    var inline1185 string = _goml_runtime_core_int_to_string(t454)
    t455 = inline1185
    var inline1182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t455)
    _goml_runtime_core_string_println(inline1182)
    var t456 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(text__0, 2, "Hé")
    var t457 string
    var inline1180 string = _goml_runtime_core_bool_to_string(t456)
    t457 = inline1180
    var inline1177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline1177)
    var t458 int = _goml_m_inherent_i_string_i_string_i_char__count(text__0)
    var t459 string
    var inline1175 string = _goml_runtime_core_int_to_string(t458)
    t459 = inline1175
    var inline1172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t459)
    _goml_runtime_core_string_println(inline1172)
    var t460 Option__string = _goml_m_inherent_i_string_i_string_i_slice__chars(text__0, 2, 7)
    var t461 string
    var inline1168 string = "none"
    switch t460.(type) {
    case Option__string_None:
        t461 = inline1168
    case Option__string_Some:
        var inline1169 string = t460.(Option__string_Some)._0
        t461 = inline1169
    default:
        panic("non-exhaustive match")
    }
    var inline1165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline1165)
    var t462 string = _goml_m_inherent_i_string_i_string_i_replace(text__0, "l", "L")
    var inline1162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t462)
    _goml_runtime_core_string_println(inline1162)
    var t463 string = _goml_m_inherent_i_string_i_string_i_repeat("ab", 3)
    var inline1159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t463)
    _goml_runtime_core_string_println(inline1159)
    var t464 bool = _goml_m_inherent_i_string_i_string_i_is__ascii(text__0)
    var t465 string
    var inline1157 string = _goml_runtime_core_bool_to_string(t464)
    t465 = inline1157
    var inline1154 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline1154)
    var t466 bool = _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case("ABC", "abc")
    var t467 string
    var inline1152 string = _goml_runtime_core_bool_to_string(t466)
    t467 = inline1152
    var inline1149 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline1149)
    var t468 string = _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase("AbC")
    var inline1146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline1146)
    var t469 string = _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase("aBc")
    var inline1143 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t469)
    _goml_runtime_core_string_println(inline1143)
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
        var inline1193 int = _goml_runtime_core_string_len(self__90)
        t488 = inline1193
        var t489 bool = start__91 < t488
        var jp485 bool
        if t489 {
            var t490 uint8
            var inline1191 uint8 = _goml_runtime_core_string_byte_get(self__90, start__91)
            t490 = inline1191
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
    var inline1197 int = _goml_runtime_core_string_len(self__90)
    t481 = inline1197
    var inline1195 string = string_byte_slice(self__90, start__91, t481)
    return inline1195
}

func _goml_m_inherent_i_string_i_string_i_trim__end(self__92 string) string {
    var end__93 int
    var inline1204 int = _goml_runtime_core_string_len(self__92)
    end__93 = inline1204
    Loop_loop496:
    for {
        var t501 bool = end__93 > 0
        var jp498 bool
        if t501 {
            var t502 int = end__93 - 1
            var t503 uint8
            var inline1199 uint8 = _goml_runtime_core_string_byte_get(self__92, t502)
            t503 = inline1199
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
    var inline1201 int = 0
    var inline1202 string = string_byte_slice(self__92, inline1201, end__93)
    return inline1202
}

func _goml_m_inherent_i_string_i_string_i_split(self__95 string, separator__96 string) *_goml_vec_string {
    var t507 [0]string = [0]string{}
    var result__97 *_goml_vec_string = func(values [0]string) *_goml_vec_string {
        return &_goml_vec_string{
            items: values[0:len(values)],
        }
    }(t507)
    var separator_len__98 int
    var inline1218 int = _goml_runtime_core_string_len(separator__96)
    separator_len__98 = inline1218
    var value_len__99 int
    var inline1216 int = _goml_runtime_core_string_len(self__95)
    value_len__99 = inline1216
    var t515 bool = separator_len__98 == 0
    if t515 {
        vec_push__Vec_6string(result__97, self__95)
        return result__97
    } else {
        var start__100 int = 0
        Loop_loop_expr510:
        for {
            var mtmp87 Option__int = string_find_from(self__95, separator__96, start__100)
            switch mtmp87.(type) {
            case Option__int_None:
                var t512 string
                var inline1210 string = string_byte_slice(self__95, start__100, value_len__99)
                t512 = inline1210
                vec_push__Vec_6string(result__97, t512)
                break Loop_loop_expr510
            case Option__int_Some:
                var x88 int = mtmp87.(Option__int_Some)._0
                var t513 string
                var inline1214 string = string_byte_slice(self__95, start__100, x88)
                t513 = inline1214
                vec_push__Vec_6string(result__97, t513)
                var t514 int = x88 + separator_len__98
                start__100 = t514
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        return result__97
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t518 int
    var inline1257 int = vec_len__Vec_6string(self__378)
    t518 = inline1257
    var parts__380 *_goml_vec_string
    var inline1255 *_goml_vec_string = vec_with_capacity__Vec_6string(t518)
    parts__380 = inline1255
    var t519 int
    var inline1253 int = vec_len__Vec_6string(self__378)
    t519 = inline1253
    var t520 FnIterator__int
    var inline1247 int = 0
    var inline1248 *ref_int_x = ref__Ref_3int(inline1247)
    var inline1249 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1248,
        end_1: t519,
    }
    var inline1250 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1249)
    }
    var inline1251 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1250)
    t520 = inline1251
    var for_iter349 FnIterator__int
    for_iter349 = t520
    Loop_loop535:
    for {
        var for_next350 Option__int
        var inline1223 func() Option__int = for_iter349.next_fn
        var inline1224 Option__int = inline1223()
        for_next350 = inline1224
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop535
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t537 string = vec_get__Vec_6string(self__378, x351)
            var t538 string
            t538 = t537
            vec_push__Vec_6string(parts__380, t538)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t522 int
    var inline1244 int = vec_len__Vec_6string(parts__380)
    t522 = inline1244
    var t523 int = t522 * 2
    var result__382 *_goml_vec_string
    var inline1242 *_goml_vec_string = vec_with_capacity__Vec_6string(t523)
    result__382 = inline1242
    var t524 int
    var inline1240 int = vec_len__Vec_6string(parts__380)
    t524 = inline1240
    var t525 FnIterator__int
    var inline1234 int = 0
    var inline1235 *ref_int_x = ref__Ref_3int(inline1234)
    var inline1236 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1235,
        end_1: t524,
    }
    var inline1237 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1236)
    }
    var inline1238 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1237)
    t525 = inline1238
    var for_iter353 FnIterator__int
    for_iter353 = t525
    Loop_loop528:
    for {
        var for_next354 Option__int
        var inline1230 func() Option__int = for_iter353.next_fn
        var inline1231 Option__int = inline1230()
        for_next354 = inline1231
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop528
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t533 bool = x355 > 0
            if t533 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t531 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t531)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t527 string = _goml_runtime_core_string_concat(result__382)
    return t527
}

func _goml_m_inherent_i_string_i_string_i_split__once(self__102 string, separator__103 string) _goml_m_Option_____o_string_c_string_q_ {
    var separator_len__104 int
    var inline1268 int = _goml_runtime_core_string_len(separator__103)
    separator_len__104 = inline1268
    var value_len__105 int
    var inline1266 int = _goml_runtime_core_string_len(self__102)
    value_len__105 = inline1266
    var t544 bool = separator_len__104 == 0
    if t544 {
        return _goml_m_Option_____o_string_c_string_q__None{}
    } else {
        var mtmp93 Option__int
        var inline1264 Option__int = string_find_from(self__102, separator__103, 0)
        mtmp93 = inline1264
        switch mtmp93.(type) {
        case Option__int_None:
            return _goml_m_Option_____o_string_c_string_q__None{}
        case Option__int_Some:
            var x94 int = mtmp93.(Option__int_Some)._0
            var t547 string
            var inline1261 int = 0
            var inline1262 string = string_byte_slice(self__102, inline1261, x94)
            t547 = inline1262
            var t548 int = x94 + separator_len__104
            var t549 string
            var inline1259 string = string_byte_slice(self__102, t548, value_len__105)
            t549 = inline1259
            var t550 Tuple2_6string_6string = Tuple2_6string_6string{
                _0: t547,
                _1: t549,
            }
            var t551 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
                _0: t550,
            }
            return t551
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
    var t578 bool
    var inline1303 int = vec_len__Vec_6string(result__108)
    var inline1304 bool = inline1303 == 0
    t578 = inline1304
    var t579 bool = !t578
    var jp577 bool
    if t579 {
        var t580 int
        var inline1270 int = vec_len__Vec_6string(result__108)
        t580 = inline1270
        var t581 int = t580 - 1
        var t582 string = vec_get__Vec_6string(result__108, t581)
        var t583 bool = t582 == ""
        jp577 = t583
    } else {
        jp577 = false
    }
    if jp577 {
        var inline1272 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(result__108)
        var inline1273 bool = inline1272 == 0
        if inline1273 {} else {
            var inline1274 int = inline1272 - 1
            vec_get__Vec_6string(result__108, inline1274)
            var inline1276 int = inline1272 - 1
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(result__108, inline1276)
        }
    } else {}
    var t559 int
    var inline1301 int = vec_len__Vec_6string(result__108)
    t559 = inline1301
    var t560 FnIterator__int
    var inline1295 int = 0
    var inline1296 *ref_int_x = ref__Ref_3int(inline1295)
    var inline1297 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1296,
        end_1: t559,
    }
    var inline1298 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1297)
    }
    var inline1299 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1298)
    t560 = inline1299
    var for_iter97 FnIterator__int
    for_iter97 = t560
    Loop_loop562:
    for {
        var for_next98 Option__int
        var inline1291 func() Option__int = for_iter97.next_fn
        var inline1292 Option__int = inline1291()
        for_next98 = inline1292
        switch for_next98.(type) {
        case Option__int_None:
            break Loop_loop562
        case Option__int_Some:
            var x99 int = for_next98.(Option__int_Some)._0
            var line__110 string = vec_get__Vec_6string(result__108, x99)
            var t570 int
            var inline1289 int = _goml_runtime_core_string_len(line__110)
            t570 = inline1289
            var t571 bool = t570 > 0
            var jp566 bool
            if t571 {
                var t572 int
                var inline1282 int = _goml_runtime_core_string_len(line__110)
                t572 = inline1282
                var t573 int = t572 - 1
                var t574 uint8
                var inline1280 uint8 = _goml_runtime_core_string_byte_get(line__110, t573)
                t574 = inline1280
                var t575 bool = t574 == 13
                jp566 = t575
            } else {
                jp566 = false
            }
            if jp566 {
                vec_get__Vec_6string(result__108, x99)
                var t567 int
                var inline1287 int = _goml_runtime_core_string_len(line__110)
                t567 = inline1287
                var t568 int = t567 - 1
                var value103 string
                var inline1284 int = 0
                var inline1285 string = string_byte_slice(line__110, inline1284, t568)
                value103 = inline1285
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
    var t586 int = vec_len__Vec_6string(self__273)
    return t586
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t589 string = _goml_runtime_core_int_to_string(self__32)
    return t589
}

func _goml_m_inherent_i_string_i_string_i_find(self__69 string, expected__70 string) Option__int {
    var t592 Option__int = string_find_from(self__69, expected__70, 0)
    return t592
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
    var inline1308 int = _goml_runtime_core_string_len(self__85)
    value_len__87 = inline1308
    var expected_len__88 int
    var inline1306 int = _goml_runtime_core_string_len(expected__86)
    expected_len__88 = inline1306
    var t601 bool = expected_len__88 > value_len__87
    if t601 {
        return Option__int_None{}
    } else {
        var start__89 int = value_len__87 - expected_len__88
        Loop_loop603:
        for {
            var t604 bool = start__89 >= 0
            if t604 {
                var t606 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(self__85, start__89, expected__86)
                if t606 {
                    var t607 Option__int = Option__int_Some{
                        _0: start__89,
                    }
                    return t607
                } else {
                    var compound_old73 int = start__89
                    var compound_value74 int = 1
                    var t608 int = compound_old73 - compound_value74
                    start__89 = t608
                    continue
                }
            } else {
                break Loop_loop603
            }
        }
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_starts__with__at(self__63 string, start__64 int, prefix__65 string) bool {
    var value_len__66 int
    var inline1320 int = _goml_runtime_core_string_len(self__63)
    value_len__66 = inline1320
    var prefix_len__67 int
    var inline1318 int = _goml_runtime_core_string_len(prefix__65)
    prefix_len__67 = inline1318
    var t631 bool = start__64 < 0
    var jp628 bool
    if t631 {
        jp628 = true
    } else {
        var t632 bool = start__64 > value_len__66
        jp628 = t632
    }
    var jp618 bool
    if jp628 {
        jp618 = true
    } else {
        var t629 int = value_len__66 - start__64
        var t630 bool = prefix_len__67 > t629
        jp618 = t630
    }
    if jp618 {
        return false
    } else {
        var end__68 int = start__64 + prefix_len__67
        var t625 bool
        var inline1316 bool = string_is_char_boundary(self__63, start__64)
        t625 = inline1316
        var jp622 bool
        if t625 {
            var inline1312 bool = string_is_char_boundary(self__63, end__68)
            jp622 = inline1312
        } else {
            jp622 = false
        }
        if jp622 {
            var t623 string
            var inline1314 string = string_byte_slice(self__63, start__64, end__68)
            t623 = inline1314
            var t624 bool = t623 == prefix__65
            return t624
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_char__count(self__73 string) int {
    var count__74 int = 0
    var t638 FnIterator__char
    var inline1326 *ref_int_x = ref__Ref_3int(0)
    var inline1327 closure_env_inherent_string_string_chars_1 = closure_env_inherent_string_string_chars_1{
        self_0: self__73,
        index_1: inline1326,
    }
    var inline1328 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(inline1327)
    }
    var inline1329 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1328)
    t638 = inline1329
    var for_iter43 FnIterator__char
    for_iter43 = t638
    Loop_loop640:
    for {
        var for_next44 Option__char
        var inline1322 func() Option__char = for_iter43.next_fn
        var inline1323 Option__char = inline1322()
        for_next44 = inline1323
        switch for_next44.(type) {
        case Option__char_None:
            break Loop_loop640
        case Option__char_Some:
            var compound_old46 int = count__74
            var compound_value47 int = 1
            var t642 int = compound_old46 + compound_value47
            count__74 = t642
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return count__74
}

func _goml_m_inherent_i_string_i_string_i_slice__chars(self__75 string, start__76 int, end__77 int) Option__string {
    var t684 bool = start__76 < 0
    var jp649 bool
    if t684 {
        jp649 = true
    } else {
        var t685 bool = end__77 < start__76
        jp649 = t685
    }
    if jp649 {
        return Option__string_None{}
    } else {
        var char_index__78 int = 0
        var t682 bool = start__76 == 0
        var jp651 Option__int
        if t682 {
            var t683 Option__int = Option__int_Some{
                _0: 0,
            }
            jp651 = t683
        } else {
            jp651 = Option__int_None{}
        }
        var start_byte__79 Option__int = jp651
        var t680 bool = end__77 == 0
        var jp653 Option__int
        if t680 {
            var t681 Option__int = Option__int_Some{
                _0: 0,
            }
            jp653 = t681
        } else {
            jp653 = Option__int_None{}
        }
        var end_byte__80 Option__int = jp653
        var t654 _goml_m_FnIterator_____o_int_c_char_q_
        var inline1341 *ref_int_x = ref__Ref_3int(0)
        var inline1342 closure_env_inherent_string_string_char_indices_2 = closure_env_inherent_string_string_char_indices_2{
            index_0: inline1341,
            self_1: self__75,
        }
        var inline1343 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
            return _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(inline1342)
        }
        var inline1344 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline1343)
        t654 = inline1344
        var for_iter50 _goml_m_FnIterator_____o_int_c_char_q_
        for_iter50 = t654
        Loop_loop670:
        for {
            var for_next51 _goml_m_Option_____o_int_c_char_q_
            var inline1331 func() _goml_m_Option_____o_int_c_char_q_ = for_iter50.next_fn
            var inline1332 _goml_m_Option_____o_int_c_char_q_ = inline1331()
            for_next51 = inline1332
            switch for_next51.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop670
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x52 Tuple2_3int_4char = for_next51.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var x54 int = x52._0
                var t678 bool = char_index__78 == start__76
                if t678 {
                    var t679 Option__int = Option__int_Some{
                        _0: x54,
                    }
                    start_byte__79 = t679
                } else {}
                var t676 bool = char_index__78 == end__77
                if t676 {
                    var t677 Option__int = Option__int_Some{
                        _0: x54,
                    }
                    end_byte__80 = t677
                } else {}
                var compound_old60 int = char_index__78
                var compound_value61 int = 1
                var t674 int = compound_old60 + compound_value61
                char_index__78 = t674
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t667 bool = char_index__78 == start__76
        if t667 {
            var t668 int
            var inline1334 int = _goml_runtime_core_string_len(self__75)
            t668 = inline1334
            var t669 Option__int = Option__int_Some{
                _0: t668,
            }
            start_byte__79 = t669
        } else {}
        var t664 bool = char_index__78 == end__77
        if t664 {
            var t665 int
            var inline1336 int = _goml_runtime_core_string_len(self__75)
            t665 = inline1336
            var t666 Option__int = Option__int_Some{
                _0: t665,
            }
            end_byte__80 = t666
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
                var t662 string
                var inline1338 string = string_byte_slice(self__75, x72, x71)
                t662 = inline1338
                var t663 Option__string = Option__string_Some{
                    _0: t662,
                }
                return t663
            default:
                return Option__string_None{}
            }
        default:
            return Option__string_None{}
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_replace(self__111 string, expected__112 string, replacement__113 string) string {
    var t694 int
    var inline1360 int = _goml_runtime_core_string_len(expected__112)
    t694 = inline1360
    var t695 bool = t694 == 0
    if t695 {
        return self__111
    } else {
        var t696 [0]string = [0]string{}
        var parts__114 *_goml_vec_string = func(values [0]string) *_goml_vec_string {
            return &_goml_vec_string{
                items: values[0:len(values)],
            }
        }(t696)
        var start__115 int = 0
        Loop_loop_expr699:
        for {
            var mtmp106 Option__int = string_find_from(self__111, expected__112, start__115)
            switch mtmp106.(type) {
            case Option__int_None:
                var t701 int
                var inline1350 int = _goml_runtime_core_string_len(self__111)
                t701 = inline1350
                var t702 string
                var inline1348 string = string_byte_slice(self__111, start__115, t701)
                t702 = inline1348
                vec_push__Vec_6string(parts__114, t702)
                break Loop_loop_expr699
            case Option__int_Some:
                var x107 int = mtmp106.(Option__int_Some)._0
                var t703 string
                var inline1358 string = string_byte_slice(self__111, start__115, x107)
                t703 = inline1358
                vec_push__Vec_6string(parts__114, t703)
                vec_push__Vec_6string(parts__114, replacement__113)
                var t704 int
                var inline1352 int = _goml_runtime_core_string_len(expected__112)
                t704 = inline1352
                var t705 int = x107 + t704
                start__115 = t705
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t698 string = _goml_runtime_core_string_concat(parts__114)
        return t698
    }
}

func _goml_m_inherent_i_string_i_string_i_repeat(self__117 string, count__118 int) string {
    var t718 bool = count__118 <= 0
    var jp711 bool
    if t718 {
        jp711 = true
    } else {
        var t719 int
        var inline1362 int = _goml_runtime_core_string_len(self__117)
        t719 = inline1362
        var t720 bool = t719 == 0
        jp711 = t720
    }
    if jp711 {
        return ""
    } else {
        var parts__119 *_goml_vec_string
        var inline1376 *_goml_vec_string = vec_with_capacity__Vec_6string(count__118)
        parts__119 = inline1376
        var t712 FnIterator__int
        var inline1370 int = 0
        var inline1371 *ref_int_x = ref__Ref_3int(inline1370)
        var inline1372 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1371,
            end_1: count__118,
        }
        var inline1373 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1372)
        }
        var inline1374 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1373)
        t712 = inline1374
        var for_iter113 FnIterator__int
        for_iter113 = t712
        Loop_loop715:
        for {
            var for_next114 Option__int
            var inline1366 func() Option__int = for_iter113.next_fn
            var inline1367 Option__int = inline1366()
            for_next114 = inline1367
            switch for_next114.(type) {
            case Option__int_None:
                break Loop_loop715
            case Option__int_Some:
                vec_push__Vec_6string(parts__119, self__117)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t714 string = _goml_runtime_core_string_concat(parts__119)
        return t714
    }
}

func _goml_m_inherent_i_string_i_string_i_is__ascii(self__120 string) bool {
    var t723 int
    var inline1390 int = _goml_runtime_core_string_len(self__120)
    t723 = inline1390
    var t724 FnIterator__int
    var inline1384 int = 0
    var inline1385 *ref_int_x = ref__Ref_3int(inline1384)
    var inline1386 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1385,
        end_1: t723,
    }
    var inline1387 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1386)
    }
    var inline1388 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1387)
    t724 = inline1388
    var for_iter117 FnIterator__int
    for_iter117 = t724
    Loop_loop726:
    for {
        var for_next118 Option__int
        var inline1380 func() Option__int = for_iter117.next_fn
        var inline1381 Option__int = inline1380()
        for_next118 = inline1381
        switch for_next118.(type) {
        case Option__int_None:
            break Loop_loop726
        case Option__int_Some:
            var x119 int = for_next118.(Option__int_Some)._0
            var t729 uint8
            var inline1378 uint8 = _goml_runtime_core_string_byte_get(self__120, x119)
            t729 = inline1378
            var t730 bool = t729 > 127
            if t730 {
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
    var t735 int
    var inline1414 int = _goml_runtime_core_string_len(self__122)
    t735 = inline1414
    var t736 int
    var inline1412 int = _goml_runtime_core_string_len(other__123)
    t736 = inline1412
    var t737 bool = t735 != t736
    if t737 {
        return false
    } else {
        var t738 int
        var inline1410 int = _goml_runtime_core_string_len(self__122)
        t738 = inline1410
        var t739 FnIterator__int
        var inline1404 int = 0
        var inline1405 *ref_int_x = ref__Ref_3int(inline1404)
        var inline1406 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1405,
            end_1: t738,
        }
        var inline1407 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1406)
        }
        var inline1408 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1407)
        t739 = inline1408
        var for_iter121 FnIterator__int
        for_iter121 = t739
        Loop_loop741:
        for {
            var for_next122 Option__int
            var inline1400 func() Option__int = for_iter121.next_fn
            var inline1401 Option__int = inline1400()
            for_next122 = inline1401
            switch for_next122.(type) {
            case Option__int_None:
                break Loop_loop741
            case Option__int_Some:
                var x123 int = for_next122.(Option__int_Some)._0
                var t744 uint8
                var inline1398 uint8 = _goml_runtime_core_string_byte_get(self__122, x123)
                t744 = inline1398
                var t745 uint8
                var inline1396 uint8 = _goml_runtime_core_string_byte_get(other__123, x123)
                t745 = inline1396
                var t746 bool
                var inline1392 uint8 = ascii_to_lowercase(t744)
                var inline1393 uint8 = ascii_to_lowercase(t745)
                var inline1394 bool = inline1392 == inline1393
                t746 = inline1394
                var t747 bool = !t746
                if t747 {
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
    var inline1432 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__125)
    values__126 = inline1432
    var t750 int
    var inline1430 int = vec_len__Vec_5uint8(values__126)
    t750 = inline1430
    var t751 FnIterator__int
    var inline1424 int = 0
    var inline1425 *ref_int_x = ref__Ref_3int(inline1424)
    var inline1426 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1425,
        end_1: t750,
    }
    var inline1427 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1426)
    }
    var inline1428 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1427)
    t751 = inline1428
    var for_iter125 FnIterator__int
    for_iter125 = t751
    var inline1417 uint8 = 97 - 65
    Loop_loop753:
    for {
        var for_next126 Option__int
        var inline1420 func() Option__int = for_iter125.next_fn
        var inline1421 Option__int = inline1420()
        for_next126 = inline1421
        switch for_next126.(type) {
        case Option__int_None:
            break Loop_loop753
        case Option__int_Some:
            var x127 int = for_next126.(Option__int_Some)._0
            vec_get__Vec_5uint8(values__126, x127)
            var t755 uint8 = vec_get__Vec_5uint8(values__126, x127)
            var value131 uint8
            var inline1416 bool = ascii_is_uppercase(t755)
            if inline1416 {
                var inline1418 uint8 = t755 + inline1417
                value131 = inline1418
            } else {
                value131 = t755
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
    var inline1450 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__129)
    values__130 = inline1450
    var t759 int
    var inline1448 int = vec_len__Vec_5uint8(values__130)
    t759 = inline1448
    var t760 FnIterator__int
    var inline1442 int = 0
    var inline1443 *ref_int_x = ref__Ref_3int(inline1442)
    var inline1444 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1443,
        end_1: t759,
    }
    var inline1445 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1444)
    }
    var inline1446 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline1445)
    t760 = inline1446
    var for_iter137 FnIterator__int
    for_iter137 = t760
    var inline1435 uint8 = 97 - 65
    Loop_loop762:
    for {
        var for_next138 Option__int
        var inline1438 func() Option__int = for_iter137.next_fn
        var inline1439 Option__int = inline1438()
        for_next138 = inline1439
        switch for_next138.(type) {
        case Option__int_None:
            break Loop_loop762
        case Option__int_Some:
            var x139 int = for_next138.(Option__int_Some)._0
            vec_get__Vec_5uint8(values__130, x139)
            var t764 uint8 = vec_get__Vec_5uint8(values__130, x139)
            var value143 uint8
            var inline1434 bool = ascii_is_lowercase(t764)
            if inline1434 {
                var inline1436 uint8 = t764 - inline1435
                value143 = inline1436
            } else {
                value143 = t764
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
    var t770 int = _goml_runtime_core_string_len(self__36)
    return t770
}

func ascii_is_whitespace(value__140 uint8) bool {
    var t787 bool = value__140 == 9
    var jp785 bool
    if t787 {
        jp785 = true
    } else {
        var t788 bool = value__140 == 10
        jp785 = t788
    }
    var jp782 bool
    if jp785 {
        jp782 = true
    } else {
        var t786 bool = value__140 == 11
        jp782 = t786
    }
    var jp779 bool
    if jp782 {
        jp779 = true
    } else {
        var t783 bool = value__140 == 12
        jp779 = t783
    }
    var jp776 bool
    if jp779 {
        jp776 = true
    } else {
        var t780 bool = value__140 == 13
        jp776 = t780
    }
    if jp776 {
        return true
    } else {
        var t777 bool = value__140 == 32
        return t777
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t791 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t791
}

func string_find_from(value__133 string, expected__134 string, start__135 int) Option__int {
    var value_len__136 int
    var inline1461 int = _goml_runtime_core_string_len(value__133)
    value_len__136 = inline1461
    var expected_len__137 int
    var inline1459 int = _goml_runtime_core_string_len(expected__134)
    expected_len__137 = inline1459
    var t819 bool = start__135 < 0
    var jp802 bool
    if t819 {
        jp802 = true
    } else {
        var t820 bool = start__135 > value_len__136
        jp802 = t820
    }
    if jp802 {
        return Option__int_None{}
    } else {
        var t805 bool = expected_len__137 == 0
        if t805 {
            var t806 Option__int = Option__int_Some{
                _0: start__135,
            }
            return t806
        } else {
            var t809 int = value_len__136 - start__135
            var t810 bool = expected_len__137 > t809
            if t810 {
                return Option__int_None{}
            } else {
                var limit__138 int = value_len__136 - expected_len__137
                var index__139 int = start__135
                Loop_loop812:
                for {
                    var t813 bool = index__139 <= limit__138
                    if t813 {
                        var t815 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(value__133, index__139, expected__134)
                        if t815 {
                            var t816 Option__int = Option__int_Some{
                                _0: index__139,
                            }
                            return t816
                        } else {
                            var compound_old149 int = index__139
                            var compound_value150 int = 1
                            var t817 int = compound_old149 + compound_value150
                            index__139 = t817
                            continue
                        }
                    } else {
                        break Loop_loop812
                    }
                }
                return Option__int_None{}
            }
        }
    }
}

func ascii_to_lowercase(value__143 uint8) uint8 {
    var t887 bool
    var inline1481 bool = value__143 >= 65
    if inline1481 {
        var inline1482 bool = value__143 <= 90
        t887 = inline1482
    } else {
        t887 = false
    }
    if t887 {
        var t888 uint8 = 97 - 65
        var t889 uint8 = value__143 + t888
        return t889
    } else {
        return value__143
    }
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop894:
    for {
        var t895 int
        var inline1484 int = _goml_runtime_core_string_len(x12)
        t895 = inline1484
        var t896 bool = index__26 < t895
        if t896 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t898 int = compound_old17 + x16
                index__26 = t898
                continue
            } else {
                var t900 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t900
            }
        } else {
            break Loop_loop894
        }
    }
    var t893 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t893
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t916 bool = string_is_char_boundary(value__21, start__22)
    var jp913 bool
    if t916 {
        var t917 bool = string_is_char_boundary(value__21, end__23)
        jp913 = t917
    } else {
        jp913 = false
    }
    if jp913 {
        var t914 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t914
    } else {
        var t915 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t915
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t920 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t920
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(self__278 *_goml_vec_string, len__279 int) struct{} {
    vec_truncate__Vec_6string(self__278, len__279)
    return struct{}{}
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t936 bool = index__16 < 0
    var jp928 bool
    if t936 {
        jp928 = true
    } else {
        var t937 int
        var inline1489 int = _goml_runtime_core_string_len(value__15)
        t937 = inline1489
        var t938 bool = index__16 > t937
        jp928 = t938
    }
    if jp928 {
        return false
    } else {
        var t931 int
        var inline1493 int = _goml_runtime_core_string_len(value__15)
        t931 = inline1493
        var t932 bool = index__16 == t931
        if t932 {
            return true
        } else {
            var t933 uint8
            var inline1491 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t933 = inline1491
            var t934_rhs uint8 = 192
            var t934 uint8 = t933 & t934_rhs
            var t935 bool = t934 != 128
            return t935
        }
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__254 func() Option__char) FnIterator__char {
    var t947 FnIterator__char = FnIterator__char{
        next_fn: next_fn__254,
    }
    return t947
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__254 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t950 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__254,
    }
    return t950
}

func ascii_is_uppercase(value__141 uint8) bool {
    var t955 bool = value__141 >= 65
    if t955 {
        var t956 bool = value__141 <= 90
        return t956
    } else {
        return false
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1075 bool = index__6 < 0
    var jp1073 bool
    if t1075 {
        jp1073 = true
    } else {
        var t1076 bool = index__6 >= length__7
        jp1073 = t1076
    }
    if jp1073 {
        var inline1495 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1495
    } else {
        var t960 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t960))
        var t963 bool = first__8 < 128
        if t963 {
            var inline1497 int = 1
            var inline1498 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1498.(type) {
            case Option__char_None:
                var inline1499 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1499
            case Option__char_Some:
                var inline1500 rune = inline1498.(Option__char_Some)._0
                var inline1502 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1500,
                    _2: inline1497,
                }
                return inline1502
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t967 bool = first__8 < 194
            if t967 {
                var inline1504 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1504
            } else {
                var t971 bool = first__8 < 224
                if t971 {
                    var t984 int = length__7 - index__6
                    var t985 bool = t984 < 2
                    if t985 {
                        var inline1506 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1506
                    } else {
                        var t973 int = index__6 + 1
                        var t974 uint8
                        var inline1520 uint8 = _goml_runtime_core_string_byte_get(value__5, t973)
                        t974 = inline1520
                        var second__9 uint32 = uint32(uint8(t974))
                        var t977 bool
                        var inline1517 bool = second__9 < 128
                        if inline1517 {
                            t977 = true
                        } else {
                            var inline1518 bool = second__9 > 191
                            t977 = inline1518
                        }
                        if t977 {
                            var inline1508 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1508
                        } else {
                            var t979_rhs uint32 = 31
                            var t979 uint32 = first__8 & t979_rhs
                            var t980_rhs int = 6
                            var t980 uint32 = t979 << t980_rhs
                            var t981_rhs uint32 = 63
                            var t981 uint32 = second__9 & t981_rhs
                            var t982 uint32 = t980 | t981
                            var inline1510 int = 2
                            var inline1511 Option__char = __goml_builtin_char_from_uint32(t982)
                            switch inline1511.(type) {
                            case Option__char_None:
                                var inline1512 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1512
                            case Option__char_Some:
                                var inline1513 rune = inline1511.(Option__char_Some)._0
                                var inline1515 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1513,
                                    _2: inline1510,
                                }
                                return inline1515
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t989 bool = first__8 < 240
                    if t989 {
                        var t1022 int = length__7 - index__6
                        var t1023 bool = t1022 < 3
                        if t1023 {
                            var inline1522 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1522
                        } else {
                            var t991 int = index__6 + 1
                            var t992 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t991)
                            var second__10 uint32 = uint32(uint8(t992))
                            var t993 int = index__6 + 2
                            var t994 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t993)
                            var third__11 uint32 = uint32(uint8(t994))
                            var t1020 bool = utf8_invalid_continuation(second__10)
                            var jp1015 bool
                            if t1020 {
                                jp1015 = true
                            } else {
                                var inline1524 bool = third__11 < 128
                                if inline1524 {
                                    jp1015 = true
                                } else {
                                    var inline1525 bool = third__11 > 191
                                    jp1015 = inline1525
                                }
                            }
                            var jp1009 bool
                            if jp1015 {
                                jp1009 = true
                            } else {
                                var t1018 bool = first__8 == 224
                                if t1018 {
                                    var t1019 bool = second__10 < 160
                                    jp1009 = t1019
                                } else {
                                    jp1009 = false
                                }
                            }
                            var jp998 bool
                            if jp1009 {
                                jp998 = true
                            } else {
                                var t1012 bool = first__8 == 237
                                if t1012 {
                                    var t1013 bool = second__10 >= 160
                                    jp998 = t1013
                                } else {
                                    jp998 = false
                                }
                            }
                            if jp998 {
                                var inline1527 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1527
                            } else {
                                var t1000_rhs uint32 = 15
                                var t1000 uint32 = first__8 & t1000_rhs
                                var t1001_rhs int = 12
                                var t1001 uint32 = t1000 << t1001_rhs
                                var t1002_rhs uint32 = 63
                                var t1002 uint32 = second__10 & t1002_rhs
                                var t1003_rhs int = 6
                                var t1003 uint32 = t1002 << t1003_rhs
                                var t1004 uint32 = t1001 | t1003
                                var t1005_rhs uint32 = 63
                                var t1005 uint32 = third__11 & t1005_rhs
                                var t1006 uint32 = t1004 | t1005
                                var inline1529 int = 3
                                var inline1530 Option__char = __goml_builtin_char_from_uint32(t1006)
                                switch inline1530.(type) {
                                case Option__char_None:
                                    var inline1531 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1531
                                case Option__char_Some:
                                    var inline1532 rune = inline1530.(Option__char_Some)._0
                                    var inline1534 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1532,
                                        _2: inline1529,
                                    }
                                    return inline1534
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1027 bool = first__8 < 245
                        if t1027 {
                            var t1068 int = length__7 - index__6
                            var t1069 bool = t1068 < 4
                            if t1069 {
                                var t1070 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1070
                            } else {
                                var t1029 int = index__6 + 1
                                var t1030 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1029)
                                var second__12 uint32 = uint32(uint8(t1030))
                                var t1031 int = index__6 + 2
                                var t1032 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1031)
                                var third__13 uint32 = uint32(uint8(t1032))
                                var t1033 int = index__6 + 3
                                var t1034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1033)
                                var fourth__14 uint32 = uint32(uint8(t1034))
                                var t1066 bool = utf8_invalid_continuation(second__12)
                                var jp1064 bool
                                if t1066 {
                                    jp1064 = true
                                } else {
                                    var t1067 bool = utf8_invalid_continuation(third__13)
                                    jp1064 = t1067
                                }
                                var jp1058 bool
                                if jp1064 {
                                    jp1058 = true
                                } else {
                                    var t1065 bool = utf8_invalid_continuation(fourth__14)
                                    jp1058 = t1065
                                }
                                var jp1052 bool
                                if jp1058 {
                                    jp1052 = true
                                } else {
                                    var t1061 bool = first__8 == 240
                                    if t1061 {
                                        var t1062 bool = second__12 < 144
                                        jp1052 = t1062
                                    } else {
                                        jp1052 = false
                                    }
                                }
                                var jp1038 bool
                                if jp1052 {
                                    jp1038 = true
                                } else {
                                    var t1055 bool = first__8 == 244
                                    if t1055 {
                                        var t1056 bool = second__12 > 143
                                        jp1038 = t1056
                                    } else {
                                        jp1038 = false
                                    }
                                }
                                if jp1038 {
                                    var t1039 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1039
                                } else {
                                    var t1040_rhs uint32 = 7
                                    var t1040 uint32 = first__8 & t1040_rhs
                                    var t1041_rhs int = 18
                                    var t1041 uint32 = t1040 << t1041_rhs
                                    var t1042_rhs uint32 = 63
                                    var t1042 uint32 = second__12 & t1042_rhs
                                    var t1043_rhs int = 12
                                    var t1043 uint32 = t1042 << t1043_rhs
                                    var t1044 uint32 = t1041 | t1043
                                    var t1045_rhs uint32 = 63
                                    var t1045 uint32 = third__13 & t1045_rhs
                                    var t1046_rhs int = 6
                                    var t1046 uint32 = t1045 << t1046_rhs
                                    var t1047 uint32 = t1044 | t1046
                                    var t1048_rhs uint32 = 63
                                    var t1048 uint32 = fourth__14 & t1048_rhs
                                    var t1049 uint32 = t1047 | t1048
                                    var t1050 Tuple3_4bool_4char_3int = utf8_valid_decode(t1049, 4)
                                    return t1050
                                }
                            }
                        } else {
                            var t1071 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1071
                        }
                    }
                }
            }
        }
    }
}

func ascii_is_lowercase(value__142 uint8) bool {
    var t1081 bool = value__142 >= 97
    if t1081 {
        var t1082 bool = value__142 <= 122
        return t1082
    } else {
        return false
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1085 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1085
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1602 rune
    var inline1538 bool = utf8_valid_scalar(value__0)
    if inline1538 {
        var inline1539 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1540 rune = inline1539._1
        commute_field1602 = inline1540
        var t1091 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1602,
            _2: width__1,
        }
        return t1091
    } else {
        var inline1536 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1536
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1096 bool = value__3 < 128
    if t1096 {
        return true
    } else {
        var t1097 bool = value__3 > 191
        return t1097
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1102 bool
    var inline1544 bool = value__30 <= 1114111
    if inline1544 {
        var inline1545 bool = value__30 >= 55296
        var inline1547 bool
        if inline1545 {
            var inline1549 bool = value__30 <= 57343
            inline1547 = inline1549
        } else {
            inline1547 = false
        }
        var inline1548 bool = !inline1547
        t1102 = inline1548
    } else {
        t1102 = false
    }
    if t1102 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1103 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1103
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1108 bool = value__4 <= 1114111
    if t1108 {
        var t1112 bool = value__4 >= 55296
        var jp1110 bool
        if t1112 {
            var t1113 bool = value__4 <= 57343
            jp1110 = t1113
        } else {
            jp1110 = false
        }
        var t1111 bool = !jp1110
        return t1111
    } else {
        return false
    }
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env425 closure_env_goml_builtin_range_0) Option__int {
    var current__496 *ref_int_x = env425.current_0
    var end__495 int = env425.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t1124 bool = value__497 < end__495
    if t1124 {
        var t1125 int = value__497 + 1
        ref_set__Ref_3int(current__496, t1125)
        var t1126 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t1126
    } else {
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(env426 closure_env_inherent_string_string_chars_1) Option__char {
    var self__52 string = env426.self_0
    var index__53 *ref_int_x = env426.index_1
    var t1129 int = ref_get__Ref_3int(index__53)
    var commute_field1605 Tuple2_4char_3int
    var inline1551 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t1129)
    var inline1552 bool = inline1551._0
    var inline1553 rune = inline1551._1
    var inline1554 int = inline1551._2
    if inline1552 {
        var inline1558 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1553,
            _1: inline1554,
        }
        commute_field1605 = inline1558
        var x32 rune = commute_field1605._0
        var x33 int = commute_field1605._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t1132 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t1132)
        var t1134 Option__char = Option__char_Some{
            _0: x32,
        }
        return t1134
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(env427 closure_env_inherent_string_string_char_indices_2) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env427.index_0
    var self__57 string = env427.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1608 Tuple2_4char_3int
    var inline1561 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1562 bool = inline1561._0
    var inline1563 rune = inline1561._1
    var inline1564 int = inline1561._2
    if inline1562 {
        var inline1568 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1563,
            _1: inline1564,
        }
        commute_field1608 = inline1568
        var x40 rune = commute_field1608._0
        var x41 int = commute_field1608._1
        var t1139 int = current__59 + x41
        ref_set__Ref_3int(index__58, t1139)
        var t1140 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t1141 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t1140,
        }
        return t1141
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
