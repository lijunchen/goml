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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int) func(int32) int32 {
    return arr[index]
}

func array_get__Array_2_5int32(arr [2]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int, value int32) [2]int32 {
    arr[index] = value
    return arr
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string struct {
    _0 Tracker
    _1 func() Record__int32
    _2 func(int32) Record__int32
    _3 func() Record__string
}

type Tracker struct {
    label string
    count *ref_int32_x
    toggled *ref_bool_x
}

type closure_env_snapshot_0 struct {
    count_0 *ref_int32_x
}

type closure_env_bump_1 struct {
    count_0 *ref_int32_x
}

type closure_env_flip_2 struct {
    toggled_0 *ref_bool_x
}

type Record__int32 interface {
    isRecord__int32()
}

type Record__int32_Value struct {
    _0 int32
}

func (_ Record__int32_Value) isRecord__int32() {}

type Record__int32_Pair struct {
    _0 int32
    _1 int32
}

func (_ Record__int32_Pair) isRecord__int32() {}

type Record__int32_Empty struct {}

func (_ Record__int32_Empty) isRecord__int32() {}

type Record__string interface {
    isRecord__string()
}

type Record__string_Value struct {
    _0 string
}

func (_ Record__string_Value) isRecord__string() {}

type Record__string_Pair struct {
    _0 string
    _1 string
}

func (_ Record__string_Pair) isRecord__string() {}

type Record__string_Empty struct {}

func (_ Record__string_Empty) isRecord__string() {}

type Maybe__int32 interface {
    isMaybe__int32()
}

type Maybe__int32_Some struct {
    _0 int32
}

func (_ Maybe__int32_Some) isMaybe__int32() {}

type Maybe__int32_None struct {}

func (_ Maybe__int32_None) isMaybe__int32() {}

type Maybe__string interface {
    isMaybe__string()
}

type Maybe__string_Some struct {
    _0 string
}

func (_ Maybe__string_Some) isMaybe__string() {}

type Maybe__string_None struct {}

func (_ Maybe__string_None) isMaybe__string() {}

func _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(self__0 Tracker) string {
    var x173 string = self__0.label
    var x174 *ref_int32_x = self__0.count
    var x175 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline348 int32 = ref_get__Ref_5int32(x174)
    current__4 = inline348
    var flag__5 bool
    var inline346 bool = ref_get__Ref_4bool(x175)
    flag__5 = inline346
    var with_label__6 string = "Tracker(" + x173
    var with_count_label__7 string = with_label__6 + ", count: "
    var t214 string
    var inline344 string = _goml_runtime_core_int32_to_string(current__4)
    t214 = inline344
    var with_count__8 string = with_count_label__7 + t214
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t215 string
    var inline342 string = _goml_runtime_core_bool_to_string(flag__5)
    t215 = inline342
    var t216 string = with_flag_label__9 + t215
    var t217 string = t216 + ")"
    return t217
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x176 int32 = self__10.(Record__int32_Value)._0
        var t222 string
        var inline350 string = _goml_runtime_core_int32_to_string(x176)
        t222 = inline350
        var t223 string = "Value(" + t222
        var t224 string = t223 + ")"
        return t224
    case Record__int32_Pair:
        var x177 int32 = self__10.(Record__int32_Pair)._0
        var x178 int32 = self__10.(Record__int32_Pair)._1
        var t225 string
        var inline354 string = _goml_runtime_core_int32_to_string(x177)
        t225 = inline354
        var prefix__14 string = "Pair(" + t225
        var t226 string = prefix__14 + ", "
        var t227 string
        var inline352 string = _goml_runtime_core_int32_to_string(x178)
        t227 = inline352
        var t228 string = t226 + t227
        var t229 string = t228 + ")"
        return t229
    case Record__int32_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x179 string = self__15.(Record__string_Value)._0
        var t234 string = "Value(" + x179
        var t235 string = t234 + ")"
        return t235
    case Record__string_Pair:
        var x180 string = self__15.(Record__string_Pair)._0
        var x181 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x180
        var t236 string = prefix__19 + ", "
        var t237 string = t236 + x181
        var t238 string = t237 + ")"
        return t238
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t241 string
    var inline356 string = _goml_runtime_core_int32_to_string(total__26)
    t241 = inline356
    var t242 string = "total: " + t241
    return t242
}

func increment(value__27 int32) int32 {
    var t245 int32 = value__27 + 1
    return t245
}

func triple(value__28 int32) int32 {
    var t248 int32 = value__28 * 3
    return t248
}

func gather(record__39 Record__int32) Maybe__int32 {
    switch record__39.(type) {
    case Record__int32_Value:
        var x191 int32 = record__39.(Record__int32_Value)._0
        var t261 Maybe__int32 = Maybe__int32_Some{
            _0: x191,
        }
        return t261
    case Record__int32_Pair:
        var x193 int32 = record__39.(Record__int32_Pair)._1
        var t262 Maybe__int32 = Maybe__int32_Some{
            _0: x193,
        }
        return t262
    case Record__int32_Empty:
        return Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var count__44 *ref_int32_x
    var inline361 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline361
    var toggled__45 *ref_bool_x
    var inline358 bool = false
    var inline359 *ref_bool_x = ref__Ref_4bool(inline358)
    toggled__45 = inline359
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var snapshot__47 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var bump__50 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var flip__53 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var t265 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
        _0: tracker__46,
        _1: func() Record__int32 {
            return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(snapshot__47)
        },
        _2: func(p0 int32) Record__int32 {
            return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(bump__50, p0)
        },
        _3: func() Record__string {
            return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(flip__53)
        },
    }
    return t265
}

func main0() struct{} {
    var mtmp196 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x197 Tracker = mtmp196._0
    var x198 func() Record__int32 = mtmp196._1
    var x199 func(int32) Record__int32 = mtmp196._2
    var x200 func() Record__string = mtmp196._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x197)
    var first_record__59 Record__int32 = x198()
    var bumped_record__60 Record__int32 = x199(5)
    var flipped_record__61 Record__string = x200()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline398 int32 = 4
    var inline399 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline400 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline401 int32 = inline399(inline398)
    var inline402 int32 = inline400(inline401)
    var inline403 [2]int32 = [2]int32{inline401, inline398}
    var inline404 [2]int32 = inline403
    var inline405 int = 1
    array_get__Array_2_5int32(inline404, inline405)
    var inline408 [2]int32 = array_set__Array_2_5int32(inline404, inline405, inline402)
    inline403 = inline408
    results__67 = inline403
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t275 bool = first_result__68 < second_result__69
    var jp268 bool
    if t275 {
        jp268 = true
    } else {
        jp268 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp270 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x201 string = stringified__65.(Maybe__string_Some)._0
        var t274 string = "Snapshot: " + x201
        jp270 = t274
    case Maybe__string_None:
        jp270 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t271 string
    var inline396 string = _goml_runtime_core_int32_to_string(first_result__68)
    t271 = inline396
    var t272 string
    var inline394 string = _goml_runtime_core_int32_to_string(second_result__69)
    t272 = inline394
    var pair_text__76 string
    var inline391 string = t271 + " -> "
    var inline392 string = inline391 + t272
    pair_text__76 = inline392
    var bool_text__77 string
    var inline384 string = _goml_runtime_core_bool_to_string(jp268)
    bool_text__77 = inline384
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline381)
    var inline378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline378)
    var inline375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline375)
    var inline372 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline372)
    var inline369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp270)
    _goml_runtime_core_string_println(inline369)
    var inline366 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline366)
    var inline363 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline363)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    if flag__20 {
        return when_true__21
    } else {
        return when_false__22
    }
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x182 int32 = value__23.(Maybe__int32_Some)._0
        var t306 string = f__24(x182)
        var t307 Maybe__string = Maybe__string_Some{
            _0: t306,
        }
        return t307
    case Maybe__int32_None:
        return Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env209 closure_env_snapshot_0) Record__int32 {
    var count__44 *ref_int32_x = env209.count_0
    var t328 int32
    var inline413 int32 = ref_get__Ref_5int32(count__44)
    t328 = inline413
    var t329 Record__int32 = Record__int32_Value{
        _0: t328,
    }
    return t329
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env210 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env210.count_0
    var before__49 int32
    var inline419 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline419
    var t332 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t332)
    var t333 int32
    var inline415 int32 = ref_get__Ref_5int32(count__44)
    t333 = inline415
    var t334 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t333,
    }
    return t334
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env211 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env211.toggled_0
    var before__51 bool
    var inline429 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline429
    var t337 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t337)
    var after__52 bool
    var inline425 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline425
    var t338 string
    var inline423 string = _goml_runtime_core_bool_to_string(before__51)
    t338 = inline423
    var t339 string
    var inline421 string = _goml_runtime_core_bool_to_string(after__52)
    t339 = inline421
    var t340 Record__string = Record__string_Pair{
        _0: t338,
        _1: t339,
    }
    return t340
}

func main() {
    main0()
}
