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
    var x183 string = self__0.label
    var x184 *ref_int32_x = self__0.count
    var x185 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline361 int32 = ref_get__Ref_5int32(x184)
    current__4 = inline361
    var flag__5 bool
    var inline359 bool = ref_get__Ref_4bool(x185)
    flag__5 = inline359
    var with_label__6 string = "Tracker(" + x183
    var with_count_label__7 string = with_label__6 + ", count: "
    var t224 string
    var inline357 string = _goml_runtime_core_int32_to_string(current__4)
    t224 = inline357
    var with_count__8 string = with_count_label__7 + t224
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t225 string
    var inline355 string = _goml_runtime_core_bool_to_string(flag__5)
    t225 = inline355
    var t226 string = with_flag_label__9 + t225
    var t227 string = t226 + ")"
    return t227
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x186 int32 = self__10.(Record__int32_Value)._0
        var t232 string
        var inline363 string = _goml_runtime_core_int32_to_string(x186)
        t232 = inline363
        var t233 string = "Value(" + t232
        var t234 string = t233 + ")"
        return t234
    case Record__int32_Pair:
        var x187 int32 = self__10.(Record__int32_Pair)._0
        var x188 int32 = self__10.(Record__int32_Pair)._1
        var t235 string
        var inline367 string = _goml_runtime_core_int32_to_string(x187)
        t235 = inline367
        var prefix__14 string = "Pair(" + t235
        var t236 string = prefix__14 + ", "
        var t237 string
        var inline365 string = _goml_runtime_core_int32_to_string(x188)
        t237 = inline365
        var t238 string = t236 + t237
        var t239 string = t238 + ")"
        return t239
    case Record__int32_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x189 string = self__15.(Record__string_Value)._0
        var t244 string = "Value(" + x189
        var t245 string = t244 + ")"
        return t245
    case Record__string_Pair:
        var x190 string = self__15.(Record__string_Pair)._0
        var x191 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x190
        var t246 string = prefix__19 + ", "
        var t247 string = t246 + x191
        var t248 string = t247 + ")"
        return t248
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t251 string
    var inline369 string = _goml_runtime_core_int32_to_string(total__26)
    t251 = inline369
    var t252 string = "total: " + t251
    return t252
}

func increment(value__27 int32) int32 {
    var t255 int32 = value__27 + 1
    return t255
}

func triple(value__28 int32) int32 {
    var t258 int32 = value__28 * 3
    return t258
}

func gather(record__39 Record__int32) Maybe__int32 {
    switch record__39.(type) {
    case Record__int32_Value:
        var x201 int32 = record__39.(Record__int32_Value)._0
        var t271 Maybe__int32 = Maybe__int32_Some{
            _0: x201,
        }
        return t271
    case Record__int32_Pair:
        var x203 int32 = record__39.(Record__int32_Pair)._1
        var t272 Maybe__int32 = Maybe__int32_Some{
            _0: x203,
        }
        return t272
    case Record__int32_Empty:
        return Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var count__44 *ref_int32_x
    var inline374 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline374
    var toggled__45 *ref_bool_x
    var inline371 bool = false
    var inline372 *ref_bool_x = ref__Ref_4bool(inline371)
    toggled__45 = inline372
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var t275 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var snapshot__47 func() Record__int32 = func() Record__int32 {
        return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(t275)
    }
    var t276 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var bump__50 func(int32) Record__int32 = func(p0 int32) Record__int32 {
        return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(t276, p0)
    }
    var t277 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var flip__53 func() Record__string = func() Record__string {
        return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(t277)
    }
    var t278 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
        _0: tracker__46,
        _1: snapshot__47,
        _2: bump__50,
        _3: flip__53,
    }
    return t278
}

func main0() struct{} {
    var mtmp206 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x207 Tracker = mtmp206._0
    var x208 func() Record__int32 = mtmp206._1
    var x209 func(int32) Record__int32 = mtmp206._2
    var x210 func() Record__string = mtmp206._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x207)
    var first_record__59 Record__int32 = x208()
    var bumped_record__60 Record__int32 = x209(5)
    var flipped_record__61 Record__string = x210()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline411 int32 = 4
    var inline412 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline413 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline414 int32 = inline412(inline411)
    var inline415 int32 = inline413(inline414)
    var inline416 [2]int32 = [2]int32{inline414, inline411}
    var inline417 [2]int32 = inline416
    var inline418 int = 1
    array_get__Array_2_5int32(inline417, inline418)
    var inline421 [2]int32 = array_set__Array_2_5int32(inline417, inline418, inline415)
    inline416 = inline421
    results__67 = inline416
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t288 bool = first_result__68 < second_result__69
    var jp281 bool
    if t288 {
        jp281 = true
    } else {
        jp281 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp283 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x211 string = stringified__65.(Maybe__string_Some)._0
        var t287 string = "Snapshot: " + x211
        jp283 = t287
    case Maybe__string_None:
        jp283 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t284 string
    var inline409 string = _goml_runtime_core_int32_to_string(first_result__68)
    t284 = inline409
    var t285 string
    var inline407 string = _goml_runtime_core_int32_to_string(second_result__69)
    t285 = inline407
    var pair_text__76 string
    var inline404 string = t284 + " -> "
    var inline405 string = inline404 + t285
    pair_text__76 = inline405
    var bool_text__77 string
    var inline397 string = _goml_runtime_core_bool_to_string(jp281)
    bool_text__77 = inline397
    var inline394 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline394)
    var inline391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline391)
    var inline388 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline388)
    var inline385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline385)
    var inline382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp283)
    _goml_runtime_core_string_println(inline382)
    var inline379 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline379)
    var inline376 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline376)
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
        var x192 int32 = value__23.(Maybe__int32_Some)._0
        var t319 string = f__24(x192)
        var t320 Maybe__string = Maybe__string_Some{
            _0: t319,
        }
        return t320
    case Maybe__int32_None:
        return Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env219 closure_env_snapshot_0) Record__int32 {
    var count__44 *ref_int32_x = env219.count_0
    var t341 int32
    var inline426 int32 = ref_get__Ref_5int32(count__44)
    t341 = inline426
    var t342 Record__int32 = Record__int32_Value{
        _0: t341,
    }
    return t342
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env220 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env220.count_0
    var before__49 int32
    var inline432 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline432
    var t345 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t345)
    var t346 int32
    var inline428 int32 = ref_get__Ref_5int32(count__44)
    t346 = inline428
    var t347 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t346,
    }
    return t347
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env221 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env221.toggled_0
    var before__51 bool
    var inline442 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline442
    var t350 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t350)
    var after__52 bool
    var inline438 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline438
    var t351 string
    var inline436 string = _goml_runtime_core_bool_to_string(before__51)
    t351 = inline436
    var t352 string
    var inline434 string = _goml_runtime_core_bool_to_string(after__52)
    t352 = inline434
    var t353 Record__string = Record__string_Pair{
        _0: t351,
        _1: t352,
    }
    return t353
}

func main() {
    main0()
}
