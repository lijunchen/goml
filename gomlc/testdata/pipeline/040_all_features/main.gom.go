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
    var inline351 int32 = ref_get__Ref_5int32(x174)
    current__4 = inline351
    var flag__5 bool
    var inline349 bool = ref_get__Ref_4bool(x175)
    flag__5 = inline349
    var with_label__6 string = "Tracker(" + x173
    var with_count_label__7 string = with_label__6 + ", count: "
    var t214 string
    var inline347 string = _goml_runtime_core_int32_to_string(current__4)
    t214 = inline347
    var with_count__8 string = with_count_label__7 + t214
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t215 string
    var inline345 string = _goml_runtime_core_bool_to_string(flag__5)
    t215 = inline345
    var t216 string = with_flag_label__9 + t215
    var t217 string = t216 + ")"
    return t217
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x176 int32 = self__10.(Record__int32_Value)._0
        var t222 string
        var inline353 string = _goml_runtime_core_int32_to_string(x176)
        t222 = inline353
        var t223 string = "Value(" + t222
        var t224 string = t223 + ")"
        return t224
    case Record__int32_Pair:
        var x177 int32 = self__10.(Record__int32_Pair)._0
        var x178 int32 = self__10.(Record__int32_Pair)._1
        var t225 string
        var inline357 string = _goml_runtime_core_int32_to_string(x177)
        t225 = inline357
        var prefix__14 string = "Pair(" + t225
        var t226 string = prefix__14 + ", "
        var t227 string
        var inline355 string = _goml_runtime_core_int32_to_string(x178)
        t227 = inline355
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
    var inline359 string = _goml_runtime_core_int32_to_string(total__26)
    t241 = inline359
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
    var inline364 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline364
    var toggled__45 *ref_bool_x
    var inline361 bool = false
    var inline362 *ref_bool_x = ref__Ref_4bool(inline361)
    toggled__45 = inline362
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var t265 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var snapshot__47 func() Record__int32 = func() Record__int32 {
        return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(t265)
    }
    var t266 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var bump__50 func(int32) Record__int32 = func(p0 int32) Record__int32 {
        return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(t266, p0)
    }
    var t267 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var flip__53 func() Record__string = func() Record__string {
        return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(t267)
    }
    var t268 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
        _0: tracker__46,
        _1: snapshot__47,
        _2: bump__50,
        _3: flip__53,
    }
    return t268
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
    var inline401 int32 = 4
    var inline402 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline403 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline404 int32 = inline402(inline401)
    var inline405 int32 = inline403(inline404)
    var inline406 [2]int32 = [2]int32{inline404, inline401}
    var inline407 [2]int32 = inline406
    var inline408 int = 1
    array_get__Array_2_5int32(inline407, inline408)
    var inline411 [2]int32 = array_set__Array_2_5int32(inline407, inline408, inline405)
    inline406 = inline411
    results__67 = inline406
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t278 bool = first_result__68 < second_result__69
    var jp271 bool
    if t278 {
        jp271 = true
    } else {
        jp271 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp273 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x201 string = stringified__65.(Maybe__string_Some)._0
        var t277 string = "Snapshot: " + x201
        jp273 = t277
    case Maybe__string_None:
        jp273 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t274 string
    var inline399 string = _goml_runtime_core_int32_to_string(first_result__68)
    t274 = inline399
    var t275 string
    var inline397 string = _goml_runtime_core_int32_to_string(second_result__69)
    t275 = inline397
    var pair_text__76 string
    var inline394 string = t274 + " -> "
    var inline395 string = inline394 + t275
    pair_text__76 = inline395
    var bool_text__77 string
    var inline387 string = _goml_runtime_core_bool_to_string(jp271)
    bool_text__77 = inline387
    var inline384 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline384)
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline381)
    var inline378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline378)
    var inline375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline375)
    var inline372 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp273)
    _goml_runtime_core_string_println(inline372)
    var inline369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline369)
    var inline366 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline366)
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
        var t309 string = f__24(x182)
        var t310 Maybe__string = Maybe__string_Some{
            _0: t309,
        }
        return t310
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
    var t331 int32
    var inline416 int32 = ref_get__Ref_5int32(count__44)
    t331 = inline416
    var t332 Record__int32 = Record__int32_Value{
        _0: t331,
    }
    return t332
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env210 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env210.count_0
    var before__49 int32
    var inline422 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline422
    var t335 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t335)
    var t336 int32
    var inline418 int32 = ref_get__Ref_5int32(count__44)
    t336 = inline418
    var t337 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t336,
    }
    return t337
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env211 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env211.toggled_0
    var before__51 bool
    var inline432 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline432
    var t340 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t340)
    var after__52 bool
    var inline428 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline428
    var t341 string
    var inline426 string = _goml_runtime_core_bool_to_string(before__51)
    t341 = inline426
    var t342 string
    var inline424 string = _goml_runtime_core_bool_to_string(after__52)
    t342 = inline424
    var t343 Record__string = Record__string_Pair{
        _0: t341,
        _1: t342,
    }
    return t343
}

func main() {
    main0()
}
