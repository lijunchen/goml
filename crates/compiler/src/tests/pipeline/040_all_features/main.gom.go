package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int32) func(int32) int32 {
    return arr[index]
}

func array_get__Array_2_5int32(arr [2]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int32, value int32) [2]int32 {
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

type Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string struct {
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

func _goml_trait_x5f_impl_x23_Describe_x23_Tracker_x23_describe(self__0 Tracker) string {
    var retv41 string
    var mtmp0 Tracker = self__0
    var x1 string = mtmp0.label
    var x2 *ref_int32_x = mtmp0.count
    var x3 *ref_bool_x = mtmp0.toggled
    var toggled__3 *ref_bool_x = x3
    var count__2 *ref_int32_x = x2
    var label__1 string = x1
    var current__4 int32 = ref_get__Ref_5int32(count__2)
    var flag__5 bool = ref_get__Ref_4bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t42 string = int32_to_string(current__4)
    var with_count__8 string = with_count_label__7 + t42
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t43 string = bool_to_string(flag__5)
    var t44 string = with_flag_label__9 + t43
    var t45 string = t44 + ")"
    retv41 = t45
    return retv41
}

func _goml_trait_x5f_impl_x23_Describe_x23_Record_x5f__x5f_int32_x23_describe(self__10 Record__int32) string {
    var retv47 string
    var jp49 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x4 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x4
        var t50 string = int32_to_string(value__11)
        var t51 string = "Value(" + t50
        var t52 string = t51 + ")"
        jp49 = t52
    case Record__int32_Pair:
        var x5 int32 = self__10.(Record__int32_Pair)._0
        var x6 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x6
        var before__12 int32 = x5
        var t53 string = int32_to_string(before__12)
        var prefix__14 string = "Pair(" + t53
        var t54 string = prefix__14 + ", "
        var t55 string = int32_to_string(after__13)
        var t56 string = t54 + t55
        var t57 string = t56 + ")"
        jp49 = t57
    case Record__int32_Empty:
        jp49 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv47 = jp49
    return retv47
}

func _goml_trait_x5f_impl_x23_Describe_x23_Record_x5f__x5f_string_x23_describe(self__15 Record__string) string {
    var retv59 string
    var jp61 string
    switch self__15.(type) {
    case Record__string_Value:
        var x7 string = self__15.(Record__string_Value)._0
        var text__16 string = x7
        var t62 string = "Value(" + text__16
        var t63 string = t62 + ")"
        jp61 = t63
    case Record__string_Pair:
        var x8 string = self__15.(Record__string_Pair)._0
        var x9 string = self__15.(Record__string_Pair)._1
        var after__18 string = x9
        var before__17 string = x8
        var prefix__19 string = "Pair(" + before__17
        var t64 string = prefix__19 + ", "
        var t65 string = t64 + after__18
        var t66 string = t65 + ")"
        jp61 = t66
    case Record__string_Empty:
        jp61 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func format_total(total__26 int32) string {
    var retv68 string
    var t69 string = int32_to_string(total__26)
    var t70 string = "total: " + t69
    retv68 = t70
    return retv68
}

func increment(value__27 int32) int32 {
    var retv72 int32
    var t73 int32 = value__27 + 1
    retv72 = t73
    return retv72
}

func triple(value__28 int32) int32 {
    var retv75 int32
    var t76 int32 = value__28 * 3
    retv75 = t76
    return retv75
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv78 string
    var mtmp11 Tuple2_6string_6string = parts__29
    var x12 string = mtmp11._0
    var x13 string = mtmp11._1
    var right__31 string = x13
    var left__30 string = x12
    var t79 string = left__30 + " -> "
    var t80 string = t79 + right__31
    retv78 = t80
    return retv78
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv82 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root14 [2]int32 = results__38
    var index15 int32 = 1
    array_get__Array_2_5int32(place_root14, index15)
    var value17 int32 = second_result__37
    var t83 [2]int32 = array_set__Array_2_5int32(place_root14, index15, value17)
    results__38 = t83
    retv82 = results__38
    return retv82
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv86 Maybe__int32
    var jp88 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x19 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x19
        var t89 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp88 = t89
    case Record__int32_Pair:
        var x21 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x21
        var t90 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp88 = t90
    case Record__int32_Empty:
        jp88 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string {
    var retv92 Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string
    var count__44 *ref_int32_x = ref__Ref_5int32(start__43)
    var toggled__45 *ref_bool_x = ref__Ref_4bool(false)
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
    var t93 Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string = Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string{
        _0: tracker__46,
        _1: func() Record__int32 {
            return _goml_inherent_x23_closure_x5f_env_x5f_snapshot_x5f_0_x23_closure_x5f_env_x5f_snapshot_x5f_0_x23_apply(snapshot__47)
        },
        _2: func(p0 int32) Record__int32 {
            return _goml_inherent_x23_closure_x5f_env_x5f_bump_x5f_1_x23_closure_x5f_env_x5f_bump_x5f_1_x23_apply(bump__50, p0)
        },
        _3: func() Record__string {
            return _goml_inherent_x23_closure_x5f_env_x5f_flip_x5f_2_x23_closure_x5f_env_x5f_flip_x5f_2_x23_apply(flip__53)
        },
    }
    retv92 = t93
    return retv92
}

func main0() struct{} {
    var mtmp24 Tuple4_7Tracker_26TFunc0_ret_13Record__int32_33TFunc1_5int32_ret_13Record__int32_27TFunc0_ret_14Record__string = build_counter("goml", 2)
    var x25 Tracker = mtmp24._0
    var x26 func() Record__int32 = mtmp24._1
    var x27 func(int32) Record__int32 = mtmp24._2
    var x28 func() Record__string = mtmp24._3
    var flip__57 func() Record__string = x28
    var bump__56 func(int32) Record__int32 = x27
    var snapshot__55 func() Record__int32 = x26
    var tracker__54 Tracker = x25
    var tracker_info__58 string = _goml_trait_x5f_impl_x23_Describe_x23_Tracker_x23_describe(tracker__54)
    var first_record__59 Record__int32 = snapshot__55()
    var bumped_record__60 Record__int32 = bump__56(5)
    var flipped_record__61 Record__string = flip__57()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_choose_x5f__x5f_T_x5f_Maybe_x5b_int32_x5d_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32 = run_transforms(4, transforms__66)
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t95 bool = first_result__68 < second_result__69
    var order_check__70 bool = t95 && true
    var first_text__71 string = _goml_trait_x5f_impl_x23_Describe_x23_Record_x5f__x5f_int32_x23_describe(first_record__59)
    var bumped_text__72 string = _goml_trait_x5f_impl_x23_Describe_x23_Record_x5f__x5f_int32_x23_describe(bumped_record__60)
    var flipped_text__73 string = _goml_trait_x5f_impl_x23_Describe_x23_Record_x5f__x5f_string_x23_describe(flipped_record__61)
    var jp97 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x29 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x29
        var t101 string = "Snapshot: " + text__74
        jp97 = t101
    case Maybe__string_None:
        jp97 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp97
    var t98 string = int32_to_string(first_result__68)
    var t99 string = int32_to_string(second_result__69)
    var t100 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t98,
        _1: t99,
    }
    var pair_text__76 string = pair_join(t100)
    var bool_text__77 string = bool_to_string(order_check__70)
    println__T_string(tracker_info__58)
    println__T_string(first_text__71)
    println__T_string(bumped_text__72)
    println__T_string(flipped_text__73)
    println__T_string(summary__75)
    println__T_string(pair_text__76)
    println__T_string(bool_text__77)
    return struct{}{}
}

func _goml_choose_x5f__x5f_T_x5f_Maybe_x5b_int32_x5d_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv103 Maybe__int32
    var jp105 Maybe__int32
    if flag__20 {
        jp105 = when_true__21
    } else {
        jp105 = when_false__22
    }
    retv103 = jp105
    return retv103
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv107 Maybe__string
    var jp109 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x10 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x10
        var t110 string = f__24(inner__25)
        var t111 Maybe__string = Maybe__string_Some{
            _0: t110,
        }
        jp109 = t111
    case Maybe__int32_None:
        jp109 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv107 = jp109
    return retv107
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_snapshot_x5f_0_x23_closure_x5f_env_x5f_snapshot_x5f_0_x23_apply(env37 closure_env_snapshot_0) Record__int32 {
    var retv115 Record__int32
    var count__44 *ref_int32_x = env37.count_0
    var t116 int32 = ref_get__Ref_5int32(count__44)
    var t117 Record__int32 = Record__int32_Value{
        _0: t116,
    }
    retv115 = t117
    return retv115
}

func _goml_inherent_x23_closure_x5f_env_x5f_bump_x5f_1_x23_closure_x5f_env_x5f_bump_x5f_1_x23_apply(env38 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv119 Record__int32
    var count__44 *ref_int32_x = env38.count_0
    var before__49 int32 = ref_get__Ref_5int32(count__44)
    var t120 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t120)
    var t121 int32 = ref_get__Ref_5int32(count__44)
    var t122 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t121,
    }
    retv119 = t122
    return retv119
}

func _goml_inherent_x23_closure_x5f_env_x5f_flip_x5f_2_x23_closure_x5f_env_x5f_flip_x5f_2_x23_apply(env39 closure_env_flip_2) Record__string {
    var retv124 Record__string
    var toggled__45 *ref_bool_x = env39.toggled_0
    var before__51 bool = ref_get__Ref_4bool(toggled__45)
    var t125 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t125)
    var after__52 bool = ref_get__Ref_4bool(toggled__45)
    var t126 string = bool_to_string(before__51)
    var t127 string = bool_to_string(after__52)
    var t128 Record__string = Record__string_Pair{
        _0: t126,
        _1: t127,
    }
    retv124 = t128
    return retv124
}

func main() {
    main0()
}
