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
    var x137 string = self__0.label
    var x138 *ref_int32_x = self__0.count
    var x139 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline312 int32 = ref_get__Ref_5int32(x138)
    current__4 = inline312
    var flag__5 bool
    var inline310 bool = ref_get__Ref_4bool(x139)
    flag__5 = inline310
    var with_label__6 string = "Tracker(" + x137
    var with_count_label__7 string = with_label__6 + ", count: "
    var t178 string
    var inline308 string = _goml_runtime_core_int32_to_string(current__4)
    t178 = inline308
    var with_count__8 string = with_count_label__7 + t178
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t179 string
    var inline306 string = _goml_runtime_core_bool_to_string(flag__5)
    t179 = inline306
    var t180 string = with_flag_label__9 + t179
    var t181 string = t180 + ")"
    return t181
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x140 int32 = self__10.(Record__int32_Value)._0
        var t186 string
        var inline314 string = _goml_runtime_core_int32_to_string(x140)
        t186 = inline314
        var t187 string = "Value(" + t186
        var t188 string = t187 + ")"
        return t188
    case Record__int32_Pair:
        var x141 int32 = self__10.(Record__int32_Pair)._0
        var x142 int32 = self__10.(Record__int32_Pair)._1
        var t189 string
        var inline318 string = _goml_runtime_core_int32_to_string(x141)
        t189 = inline318
        var prefix__14 string = "Pair(" + t189
        var t190 string = prefix__14 + ", "
        var t191 string
        var inline316 string = _goml_runtime_core_int32_to_string(x142)
        t191 = inline316
        var t192 string = t190 + t191
        var t193 string = t192 + ")"
        return t193
    case Record__int32_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x143 string = self__15.(Record__string_Value)._0
        var t198 string = "Value(" + x143
        var t199 string = t198 + ")"
        return t199
    case Record__string_Pair:
        var x144 string = self__15.(Record__string_Pair)._0
        var x145 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x144
        var t200 string = prefix__19 + ", "
        var t201 string = t200 + x145
        var t202 string = t201 + ")"
        return t202
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t205 string
    var inline320 string = _goml_runtime_core_int32_to_string(total__26)
    t205 = inline320
    var t206 string = "total: " + t205
    return t206
}

func increment(value__27 int32) int32 {
    var t209 int32 = value__27 + 1
    return t209
}

func triple(value__28 int32) int32 {
    var t212 int32 = value__28 * 3
    return t212
}

func gather(record__39 Record__int32) Maybe__int32 {
    switch record__39.(type) {
    case Record__int32_Value:
        var x155 int32 = record__39.(Record__int32_Value)._0
        var t225 Maybe__int32 = Maybe__int32_Some{
            _0: x155,
        }
        return t225
    case Record__int32_Pair:
        var x157 int32 = record__39.(Record__int32_Pair)._1
        var t226 Maybe__int32 = Maybe__int32_Some{
            _0: x157,
        }
        return t226
    case Record__int32_Empty:
        return Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var count__44 *ref_int32_x
    var inline325 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline325
    var toggled__45 *ref_bool_x
    var inline322 bool = false
    var inline323 *ref_bool_x = ref__Ref_4bool(inline322)
    toggled__45 = inline323
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
    var t229 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    return t229
}

func main0() struct{} {
    var mtmp160 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x161 Tracker = mtmp160._0
    var x162 func() Record__int32 = mtmp160._1
    var x163 func(int32) Record__int32 = mtmp160._2
    var x164 func() Record__string = mtmp160._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x161)
    var first_record__59 Record__int32 = x162()
    var bumped_record__60 Record__int32 = x163(5)
    var flipped_record__61 Record__string = x164()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline362 int32 = 4
    var inline363 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline364 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline365 int32 = inline363(inline362)
    var inline366 int32 = inline364(inline365)
    var inline367 [2]int32 = [2]int32{inline365, inline362}
    var inline368 [2]int32 = inline367
    var inline369 int = 1
    array_get__Array_2_5int32(inline368, inline369)
    var inline372 [2]int32 = array_set__Array_2_5int32(inline368, inline369, inline366)
    inline367 = inline372
    results__67 = inline367
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t239 bool = first_result__68 < second_result__69
    var jp232 bool
    if t239 {
        jp232 = true
    } else {
        jp232 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp234 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x165 string = stringified__65.(Maybe__string_Some)._0
        var t238 string = "Snapshot: " + x165
        jp234 = t238
    case Maybe__string_None:
        jp234 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t235 string
    var inline360 string = _goml_runtime_core_int32_to_string(first_result__68)
    t235 = inline360
    var t236 string
    var inline358 string = _goml_runtime_core_int32_to_string(second_result__69)
    t236 = inline358
    var pair_text__76 string
    var inline355 string = t235 + " -> "
    var inline356 string = inline355 + t236
    pair_text__76 = inline356
    var bool_text__77 string
    var inline348 string = _goml_runtime_core_bool_to_string(jp232)
    bool_text__77 = inline348
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline345)
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline342)
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline339)
    var inline336 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline336)
    var inline333 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp234)
    _goml_runtime_core_string_println(inline333)
    var inline330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline330)
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline327)
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
        var x146 int32 = value__23.(Maybe__int32_Some)._0
        var t270 string = f__24(x146)
        var t271 Maybe__string = Maybe__string_Some{
            _0: t270,
        }
        return t271
    case Maybe__int32_None:
        return Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env173 closure_env_snapshot_0) Record__int32 {
    var count__44 *ref_int32_x = env173.count_0
    var t292 int32
    var inline377 int32 = ref_get__Ref_5int32(count__44)
    t292 = inline377
    var t293 Record__int32 = Record__int32_Value{
        _0: t292,
    }
    return t293
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env174 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env174.count_0
    var before__49 int32
    var inline383 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline383
    var t296 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t296)
    var t297 int32
    var inline379 int32 = ref_get__Ref_5int32(count__44)
    t297 = inline379
    var t298 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t297,
    }
    return t298
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env175 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env175.toggled_0
    var before__51 bool
    var inline393 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline393
    var t301 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t301)
    var after__52 bool
    var inline389 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline389
    var t302 string
    var inline387 string = _goml_runtime_core_bool_to_string(before__51)
    t302 = inline387
    var t303 string
    var inline385 string = _goml_runtime_core_bool_to_string(after__52)
    t303 = inline385
    var t304 Record__string = Record__string_Pair{
        _0: t302,
        _1: t303,
    }
    return t304
}

func main() {
    main0()
}
