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

type Ordering int32

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
    var x409 string = self__0.label
    var x410 *ref_int32_x = self__0.count
    var x411 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline587 int32 = ref_get__Ref_5int32(x410)
    current__4 = inline587
    var flag__5 bool
    var inline585 bool = ref_get__Ref_4bool(x411)
    flag__5 = inline585
    var with_label__6 string = "Tracker(" + x409
    var with_count_label__7 string = with_label__6 + ", count: "
    var t450 string
    var inline583 string = _goml_runtime_core_int32_to_string(current__4)
    t450 = inline583
    var with_count__8 string = with_count_label__7 + t450
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t451 string
    var inline581 string = _goml_runtime_core_bool_to_string(flag__5)
    t451 = inline581
    var t452 string = with_flag_label__9 + t451
    var t453 string = t452 + ")"
    return t453
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    switch self__10.(type) {
    case Record__int32_Value:
        var x412 int32 = self__10.(Record__int32_Value)._0
        var t458 string
        var inline589 string = _goml_runtime_core_int32_to_string(x412)
        t458 = inline589
        var t459 string = "Value(" + t458
        var t460 string = t459 + ")"
        return t460
    case Record__int32_Pair:
        var x413 int32 = self__10.(Record__int32_Pair)._0
        var x414 int32 = self__10.(Record__int32_Pair)._1
        var t461 string
        var inline593 string = _goml_runtime_core_int32_to_string(x413)
        t461 = inline593
        var prefix__14 string = "Pair(" + t461
        var t462 string = prefix__14 + ", "
        var t463 string
        var inline591 string = _goml_runtime_core_int32_to_string(x414)
        t463 = inline591
        var t464 string = t462 + t463
        var t465 string = t464 + ")"
        return t465
    case Record__int32_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x415 string = self__15.(Record__string_Value)._0
        var t470 string = "Value(" + x415
        var t471 string = t470 + ")"
        return t471
    case Record__string_Pair:
        var x416 string = self__15.(Record__string_Pair)._0
        var x417 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x416
        var t472 string = prefix__19 + ", "
        var t473 string = t472 + x417
        var t474 string = t473 + ")"
        return t474
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t477 string
    var inline595 string = _goml_runtime_core_int32_to_string(total__26)
    t477 = inline595
    var t478 string = "total: " + t477
    return t478
}

func increment(value__27 int32) int32 {
    var t481 int32 = value__27 + 1
    return t481
}

func triple(value__28 int32) int32 {
    var t484 int32 = value__28 * 3
    return t484
}

func gather(record__39 Record__int32) Maybe__int32 {
    switch record__39.(type) {
    case Record__int32_Value:
        var x427 int32 = record__39.(Record__int32_Value)._0
        var t497 Maybe__int32 = Maybe__int32_Some{
            _0: x427,
        }
        return t497
    case Record__int32_Pair:
        var x429 int32 = record__39.(Record__int32_Pair)._1
        var t498 Maybe__int32 = Maybe__int32_Some{
            _0: x429,
        }
        return t498
    case Record__int32_Empty:
        return Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var count__44 *ref_int32_x
    var inline600 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline600
    var toggled__45 *ref_bool_x
    var inline597 bool = false
    var inline598 *ref_bool_x = ref__Ref_4bool(inline597)
    toggled__45 = inline598
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var t501 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var snapshot__47 func() Record__int32 = func() Record__int32 {
        return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(t501)
    }
    var t502 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var bump__50 func(int32) Record__int32 = func(p0 int32) Record__int32 {
        return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(t502, p0)
    }
    var t503 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var flip__53 func() Record__string = func() Record__string {
        return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(t503)
    }
    var t504 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
        _0: tracker__46,
        _1: snapshot__47,
        _2: bump__50,
        _3: flip__53,
    }
    return t504
}

func main0() struct{} {
    var mtmp432 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x433 Tracker = mtmp432._0
    var x434 func() Record__int32 = mtmp432._1
    var x435 func(int32) Record__int32 = mtmp432._2
    var x436 func() Record__string = mtmp432._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x433)
    var first_record__59 Record__int32 = x434()
    var bumped_record__60 Record__int32 = x435(5)
    var flipped_record__61 Record__string = x436()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline637 int32 = 4
    var inline638 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline639 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline640 int32 = inline638(inline637)
    var inline641 int32 = inline639(inline640)
    var inline642 [2]int32 = [2]int32{inline640, inline637}
    var inline643 [2]int32 = inline642
    var inline644 int = 1
    array_get__Array_2_5int32(inline643, inline644)
    var inline647 [2]int32 = array_set__Array_2_5int32(inline643, inline644, inline641)
    inline642 = inline647
    results__67 = inline642
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t514 bool = first_result__68 < second_result__69
    var jp507 bool
    if t514 {
        jp507 = true
    } else {
        jp507 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp509 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x437 string = stringified__65.(Maybe__string_Some)._0
        var t513 string = "Snapshot: " + x437
        jp509 = t513
    case Maybe__string_None:
        jp509 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t510 string
    var inline635 string = _goml_runtime_core_int32_to_string(first_result__68)
    t510 = inline635
    var t511 string
    var inline633 string = _goml_runtime_core_int32_to_string(second_result__69)
    t511 = inline633
    var pair_text__76 string
    var inline630 string = t510 + " -> "
    var inline631 string = inline630 + t511
    pair_text__76 = inline631
    var bool_text__77 string
    var inline623 string = _goml_runtime_core_bool_to_string(jp507)
    bool_text__77 = inline623
    var inline620 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline620)
    var inline617 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline617)
    var inline614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline614)
    var inline611 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline611)
    var inline608 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp509)
    _goml_runtime_core_string_println(inline608)
    var inline605 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline605)
    var inline602 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline602)
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
        var x418 int32 = value__23.(Maybe__int32_Some)._0
        var t545 string = f__24(x418)
        var t546 Maybe__string = Maybe__string_Some{
            _0: t545,
        }
        return t546
    case Maybe__int32_None:
        return Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env445 closure_env_snapshot_0) Record__int32 {
    var count__44 *ref_int32_x = env445.count_0
    var t567 int32
    var inline652 int32 = ref_get__Ref_5int32(count__44)
    t567 = inline652
    var t568 Record__int32 = Record__int32_Value{
        _0: t567,
    }
    return t568
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env446 closure_env_bump_1, delta__48 int32) Record__int32 {
    var count__44 *ref_int32_x = env446.count_0
    var before__49 int32
    var inline658 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline658
    var t571 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t571)
    var t572 int32
    var inline654 int32 = ref_get__Ref_5int32(count__44)
    t572 = inline654
    var t573 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t572,
    }
    return t573
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env447 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env447.toggled_0
    var before__51 bool
    var inline668 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline668
    var t576 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t576)
    var after__52 bool
    var inline664 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline664
    var t577 string
    var inline662 string = _goml_runtime_core_bool_to_string(before__51)
    t577 = inline662
    var t578 string
    var inline660 string = _goml_runtime_core_bool_to_string(after__52)
    t578 = inline660
    var t579 Record__string = Record__string_Pair{
        _0: t577,
        _1: t578,
    }
    return t579
}

func main() {
    main0()
}
