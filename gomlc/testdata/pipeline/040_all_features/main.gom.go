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
    var x156 string = self__0.label
    var x157 *ref_int32_x = self__0.count
    var x158 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline331 int32 = ref_get__Ref_5int32(x157)
    current__4 = inline331
    var flag__5 bool
    var inline329 bool = ref_get__Ref_4bool(x158)
    flag__5 = inline329
    var with_label__6 string = "Tracker(" + x156
    var with_count_label__7 string = with_label__6 + ", count: "
    var t197 string
    var inline327 string = _goml_runtime_core_int32_to_string(current__4)
    t197 = inline327
    var with_count__8 string = with_count_label__7 + t197
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t198 string
    var inline325 string = _goml_runtime_core_bool_to_string(flag__5)
    t198 = inline325
    var t199 string = with_flag_label__9 + t198
    var t200 string = t199 + ")"
    return t200
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x159 int32 = self__10.(Record__int32_Value)._0
        var t205 string
        var inline333 string = _goml_runtime_core_int32_to_string(x159)
        t205 = inline333
        var t206 string = "Value(" + t205
        var t207 string = t206 + ")"
        return t207
    case Record__int32_Pair:
        var x160 int32 = self__10.(Record__int32_Pair)._0
        var x161 int32 = self__10.(Record__int32_Pair)._1
        var t208 string
        var inline337 string = _goml_runtime_core_int32_to_string(x160)
        t208 = inline337
        var prefix__14 string = "Pair(" + t208
        var t209 string = prefix__14 + ", "
        var t210 string
        var inline335 string = _goml_runtime_core_int32_to_string(x161)
        t210 = inline335
        var t211 string = t209 + t210
        var t212 string = t211 + ")"
        return t212
    case Record__int32_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x162 string = self__15.(Record__string_Value)._0
        var t217 string = "Value(" + x162
        var t218 string = t217 + ")"
        return t218
    case Record__string_Pair:
        var x163 string = self__15.(Record__string_Pair)._0
        var x164 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x163
        var t219 string = prefix__19 + ", "
        var t220 string = t219 + x164
        var t221 string = t220 + ")"
        return t221
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t224 string
    var inline339 string = _goml_runtime_core_int32_to_string(total__26)
    t224 = inline339
    var t225 string = "total: " + t224
    return t225
}

func increment(value__27 int32) int32 {
    var t228 int32 = value__27 + 1
    return t228
}

func triple(value__28 int32) int32 {
    var t231 int32 = value__28 * 3
    return t231
}

func gather(record__39 Record__int32) Maybe__int32 {
    switch record__39.(type) {
    case Record__int32_Value:
        var x174 int32 = record__39.(Record__int32_Value)._0
        var t244 Maybe__int32 = Maybe__int32_Some{
            _0: x174,
        }
        return t244
    case Record__int32_Pair:
        var x176 int32 = record__39.(Record__int32_Pair)._1
        var t245 Maybe__int32 = Maybe__int32_Some{
            _0: x176,
        }
        return t245
    case Record__int32_Empty:
        return Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var count__44 *ref_int32_x
    var inline344 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline344
    var toggled__45 *ref_bool_x
    var inline341 bool = false
    var inline342 *ref_bool_x = ref__Ref_4bool(inline341)
    toggled__45 = inline342
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
    var t248 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    return t248
}

func main0() struct{} {
    var mtmp179 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x180 Tracker = mtmp179._0
    var x181 func() Record__int32 = mtmp179._1
    var x182 func(int32) Record__int32 = mtmp179._2
    var x183 func() Record__string = mtmp179._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x180)
    var first_record__59 Record__int32 = x181()
    var bumped_record__60 Record__int32 = x182(5)
    var flipped_record__61 Record__string = x183()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline381 int32 = 4
    var inline382 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline383 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline384 int32 = inline382(inline381)
    var inline385 int32 = inline383(inline384)
    var inline386 [2]int32 = [2]int32{inline384, inline381}
    var inline387 [2]int32 = inline386
    var inline388 int = 1
    array_get__Array_2_5int32(inline387, inline388)
    var inline391 [2]int32 = array_set__Array_2_5int32(inline387, inline388, inline385)
    inline386 = inline391
    results__67 = inline386
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t258 bool = first_result__68 < second_result__69
    var jp251 bool
    if t258 {
        jp251 = true
    } else {
        jp251 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp253 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x184 string = stringified__65.(Maybe__string_Some)._0
        var t257 string = "Snapshot: " + x184
        jp253 = t257
    case Maybe__string_None:
        jp253 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t254 string
    var inline379 string = _goml_runtime_core_int32_to_string(first_result__68)
    t254 = inline379
    var t255 string
    var inline377 string = _goml_runtime_core_int32_to_string(second_result__69)
    t255 = inline377
    var pair_text__76 string
    var inline374 string = t254 + " -> "
    var inline375 string = inline374 + t255
    pair_text__76 = inline375
    var bool_text__77 string
    var inline367 string = _goml_runtime_core_bool_to_string(jp251)
    bool_text__77 = inline367
    var inline364 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline364)
    var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline361)
    var inline358 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline358)
    var inline355 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline355)
    var inline352 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp253)
    _goml_runtime_core_string_println(inline352)
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline349)
    var inline346 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline346)
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
        var x165 int32 = value__23.(Maybe__int32_Some)._0
        var t289 string = f__24(x165)
        var t290 Maybe__string = Maybe__string_Some{
            _0: t289,
        }
        return t290
    case Maybe__int32_None:
        return Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env192 closure_env_snapshot_0) Record__int32 {
    var count__44 *ref_int32_x = env192.count_0
    var t311 int32
    var inline396 int32 = ref_get__Ref_5int32(count__44)
    t311 = inline396
    var t312 Record__int32 = Record__int32_Value{
        _0: t311,
    }
    return t312
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env193 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env193.count_0
    var before__49 int32
    var inline402 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline402
    var t315 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t315)
    var t316 int32
    var inline398 int32 = ref_get__Ref_5int32(count__44)
    t316 = inline398
    var t317 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t316,
    }
    return t317
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env194 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env194.toggled_0
    var before__51 bool
    var inline412 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline412
    var t320 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t320)
    var after__52 bool
    var inline408 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline408
    var t321 string
    var inline406 string = _goml_runtime_core_bool_to_string(before__51)
    t321 = inline406
    var t322 string
    var inline404 string = _goml_runtime_core_bool_to_string(after__52)
    t322 = inline404
    var t323 Record__string = Record__string_Pair{
        _0: t321,
        _1: t322,
    }
    return t323
}

func main() {
    main0()
}
