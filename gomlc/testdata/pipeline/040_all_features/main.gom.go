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

type Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string struct {
    _0 Tracker
    _1 func() Record__i32
    _2 func(int32) Record__i32
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

type Record__i32 struct {
    _tag int32
    _v0_0 int32
    _v1_0 int32
    _v1_1 int32
}

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

type Maybe__i32 struct {
    _tag int32
    _v0_0 int32
}

type Maybe__string struct {
    _tag int32
    _v0_0 string
}

func _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(self__0 Tracker) string {
    var x412 string = self__0.label
    var x413 *ref_int32_x = self__0.count
    var x414 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline590 int32 = ref_get__Ref_5int32(x413)
    current__4 = inline590
    var flag__5 bool
    var inline588 bool = ref_get__Ref_4bool(x414)
    flag__5 = inline588
    var with_label__6 string = "Tracker(" + x412
    var with_count_label__7 string = with_label__6 + ", count: "
    var t453 string
    var inline586 string = _goml_runtime_core_int32_to_string(current__4)
    t453 = inline586
    var with_count__8 string = with_count_label__7 + t453
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t454 string
    var inline584 string = _goml_runtime_core_bool_to_string(flag__5)
    t454 = inline584
    var t455 string = with_flag_label__9 + t454
    var t456 string = t455 + ")"
    return t456
}

func _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(self__10 Record__i32) string {
    switch self__10._tag {
    case 0:
        var x415 int32 = self__10._v0_0
        var t461 string
        var inline592 string = _goml_runtime_core_int32_to_string(x415)
        t461 = inline592
        var t462 string = "Value(" + t461
        var t463 string = t462 + ")"
        return t463
    case 1:
        var x416 int32 = self__10._v1_0
        var x417 int32 = self__10._v1_1
        var t464 string
        var inline596 string = _goml_runtime_core_int32_to_string(x416)
        t464 = inline596
        var prefix__14 string = "Pair(" + t464
        var t465 string = prefix__14 + ", "
        var t466 string
        var inline594 string = _goml_runtime_core_int32_to_string(x417)
        t466 = inline594
        var t467 string = t465 + t466
        var t468 string = t467 + ")"
        return t468
    case 2:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x418 string = self__15.(Record__string_Value)._0
        var t473 string = "Value(" + x418
        var t474 string = t473 + ")"
        return t474
    case Record__string_Pair:
        var x419 string = self__15.(Record__string_Pair)._0
        var x420 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x419
        var t475 string = prefix__19 + ", "
        var t476 string = t475 + x420
        var t477 string = t476 + ")"
        return t477
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t480 string
    var inline598 string = _goml_runtime_core_int32_to_string(total__26)
    t480 = inline598
    var t481 string = "total: " + t480
    return t481
}

func increment(value__27 int32) int32 {
    var t484 int32 = value__27 + 1
    return t484
}

func triple(value__28 int32) int32 {
    var t487 int32 = value__28 * 3
    return t487
}

func gather(record__39 Record__i32) Maybe__i32 {
    switch record__39._tag {
    case 0:
        var x430 int32 = record__39._v0_0
        var t500 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: x430,
        }
        return t500
    case 1:
        var x432 int32 = record__39._v1_1
        var t501 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: x432,
        }
        return t501
    case 2:
        return Maybe__i32{
            _tag: 1,
        }
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string {
    var count__44 *ref_int32_x
    var inline603 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline603
    var toggled__45 *ref_bool_x
    var inline600 bool = false
    var inline601 *ref_bool_x = ref__Ref_4bool(inline600)
    toggled__45 = inline601
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var t504 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var snapshot__47 func() Record__i32 = func() Record__i32 {
        return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(t504)
    }
    var t505 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var bump__50 func(int32) Record__i32 = func(p0 int32) Record__i32 {
        return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(t505, p0)
    }
    var t506 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var flip__53 func() Record__string = func() Record__string {
        return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(t506)
    }
    var t507 Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string = Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string{
        _0: tracker__46,
        _1: snapshot__47,
        _2: bump__50,
        _3: flip__53,
    }
    return t507
}

func main0() struct{} {
    var mtmp435 Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string = build_counter("goml", 2)
    var x436 Tracker = mtmp435._0
    var x437 func() Record__i32 = mtmp435._1
    var x438 func(int32) Record__i32 = mtmp435._2
    var x439 func() Record__string = mtmp435._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x436)
    var first_record__59 Record__i32 = x437()
    var bumped_record__60 Record__i32 = x438(5)
    var flipped_record__61 Record__string = x439()
    var maybe_first__62 Maybe__i32 = gather(first_record__59)
    var maybe_second__63 Maybe__i32 = gather(bumped_record__60)
    var chosen__64 Maybe__i32 = _goml_m_choose____T__Maybe_l_i32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_i32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline640 int32 = 4
    var inline641 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline642 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline643 int32 = inline641(inline640)
    var inline644 int32 = inline642(inline643)
    var inline645 [2]int32 = [2]int32{inline643, inline640}
    var inline646 [2]int32 = inline645
    var inline647 int = 1
    array_get__Array_2_5int32(inline646, inline647)
    var inline650 [2]int32 = array_set__Array_2_5int32(inline646, inline647, inline644)
    inline645 = inline650
    results__67 = inline645
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t517 bool = first_result__68 < second_result__69
    var jp510 bool
    if t517 {
        jp510 = true
    } else {
        jp510 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp512 string
    switch stringified__65._tag {
    case 0:
        var x440 string = stringified__65._v0_0
        var t516 string = "Snapshot: " + x440
        jp512 = t516
    case 1:
        jp512 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t513 string
    var inline638 string = _goml_runtime_core_int32_to_string(first_result__68)
    t513 = inline638
    var t514 string
    var inline636 string = _goml_runtime_core_int32_to_string(second_result__69)
    t514 = inline636
    var pair_text__76 string
    var inline633 string = t513 + " -> "
    var inline634 string = inline633 + t514
    pair_text__76 = inline634
    var bool_text__77 string
    var inline626 string = _goml_runtime_core_bool_to_string(jp510)
    bool_text__77 = inline626
    var inline623 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline623)
    var inline620 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline620)
    var inline617 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline617)
    var inline614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline614)
    var inline611 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp512)
    _goml_runtime_core_string_println(inline611)
    var inline608 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline608)
    var inline605 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline605)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_i32_r_(flag__20 bool, when_true__21 Maybe__i32, when_false__22 Maybe__i32) Maybe__i32 {
    if flag__20 {
        return when_true__21
    } else {
        return when_false__22
    }
}

func map_maybe__T_i32__U_string(value__23 Maybe__i32, f__24 func(int32) string) Maybe__string {
    switch value__23._tag {
    case 0:
        var x421 int32 = value__23._v0_0
        var t548 string = f__24(x421)
        var t549 Maybe__string = Maybe__string{
            _tag: 0,
            _v0_0: t548,
        }
        return t549
    case 1:
        return Maybe__string{
            _tag: 1,
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env448 closure_env_snapshot_0) Record__i32 {
    var count__44 *ref_int32_x = env448.count_0
    var t570 int32
    var inline655 int32 = ref_get__Ref_5int32(count__44)
    t570 = inline655
    var t571 Record__i32 = Record__i32{
        _tag: 0,
        _v0_0: t570,
    }
    return t571
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env449 closure_env_bump_1, delta__48 int32) Record__i32 {
    var count__44 *ref_int32_x = env449.count_0
    var before__49 int32
    var inline661 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline661
    var t574 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t574)
    var t575 int32
    var inline657 int32 = ref_get__Ref_5int32(count__44)
    t575 = inline657
    var t576 Record__i32 = Record__i32{
        _tag: 1,
        _v1_0: before__49,
        _v1_1: t575,
    }
    return t576
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env450 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env450.toggled_0
    var before__51 bool
    var inline671 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline671
    var t579 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t579)
    var after__52 bool
    var inline667 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline667
    var t580 string
    var inline665 string = _goml_runtime_core_bool_to_string(before__51)
    t580 = inline665
    var t581 string
    var inline663 string = _goml_runtime_core_bool_to_string(after__52)
    t581 = inline663
    var t582 Record__string = Record__string_Pair{
        _0: t580,
        _1: t581,
    }
    return t582
}

func main() {
    main0()
}
