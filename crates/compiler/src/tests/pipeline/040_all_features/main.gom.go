package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_Fn_int32_to_int32(arr [2]func(int32) int32, index int32) func(int32) int32 {
    return arr[index]
}

func array_get__Array_2_int32(arr [2]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_2_int32(arr [2]int32, index int32, value int32) [2]int32 {
    arr[index] = value
    return arr
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_string_string struct {
    _0 string
    _1 string
}

type Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string struct {
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

type GoError = error

func _goml_trait_impl_Describe_Tracker_describe(self__0 Tracker) string {
    var retv36 string
    var mtmp0 Tracker = self__0
    var x1 string = mtmp0.label
    var x2 *ref_int32_x = mtmp0.count
    var x3 *ref_bool_x = mtmp0.toggled
    var toggled__3 *ref_bool_x = x3
    var count__2 *ref_int32_x = x2
    var label__1 string = x1
    var current__4 int32 = ref_get__Ref_int32(count__2)
    var flag__5 bool = ref_get__Ref_bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t37 string = int32_to_string(current__4)
    var with_count__8 string = with_count_label__7 + t37
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t38 string = bool_to_string(flag__5)
    var t39 string = with_flag_label__9 + t38
    var t40 string = t39 + ")"
    retv36 = t40
    return retv36
}

func _goml_trait_impl_Describe_Record__int32_describe(self__10 Record__int32) string {
    var retv42 string
    var jp44 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x4 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x4
        var t45 string = int32_to_string(value__11)
        var t46 string = "Value(" + t45
        var t47 string = t46 + ")"
        jp44 = t47
    case Record__int32_Pair:
        var x5 int32 = self__10.(Record__int32_Pair)._0
        var x6 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x6
        var before__12 int32 = x5
        var t48 string = int32_to_string(before__12)
        var prefix__14 string = "Pair(" + t48
        var t49 string = prefix__14 + ", "
        var t50 string = int32_to_string(after__13)
        var t51 string = t49 + t50
        var t52 string = t51 + ")"
        jp44 = t52
    case Record__int32_Empty:
        jp44 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func _goml_trait_impl_Describe_Record__string_describe(self__15 Record__string) string {
    var retv54 string
    var jp56 string
    switch self__15.(type) {
    case Record__string_Value:
        var x7 string = self__15.(Record__string_Value)._0
        var text__16 string = x7
        var t57 string = "Value(" + text__16
        var t58 string = t57 + ")"
        jp56 = t58
    case Record__string_Pair:
        var x8 string = self__15.(Record__string_Pair)._0
        var x9 string = self__15.(Record__string_Pair)._1
        var after__18 string = x9
        var before__17 string = x8
        var prefix__19 string = "Pair(" + before__17
        var t59 string = prefix__19 + ", "
        var t60 string = t59 + after__18
        var t61 string = t60 + ")"
        jp56 = t61
    case Record__string_Empty:
        jp56 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func format_total(total__26 int32) string {
    var retv63 string
    var t64 string = int32_to_string(total__26)
    var t65 string = "total: " + t64
    retv63 = t65
    return retv63
}

func increment(value__27 int32) int32 {
    var retv67 int32
    var t68 int32 = value__27 + 1
    retv67 = t68
    return retv67
}

func triple(value__28 int32) int32 {
    var retv70 int32
    var t71 int32 = value__28 * 3
    retv70 = t71
    return retv70
}

func pair_join(parts__29 Tuple2_string_string) string {
    var retv73 string
    var mtmp11 Tuple2_string_string = parts__29
    var x12 string = mtmp11._0
    var x13 string = mtmp11._1
    var right__31 string = x13
    var left__30 string = x12
    var t74 string = left__30 + " -> "
    var t75 string = t74 + right__31
    retv73 = t75
    return retv73
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv77 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var t78 [2]int32 = [2]int32{first_result__36, value__32}
    var t79 [2]int32 = array_set__Array_2_int32(t78, 1, second_result__37)
    retv77 = t79
    return retv77
}

func gather__native(record__38 Record__int32) (int32, bool) {
    switch record__38.(type) {
    case Record__int32_Value:
        var x14 int32 = record__38.(Record__int32_Value)._0
        var value__39 int32 = x14
        return value__39, true
    case Record__int32_Pair:
        var x16 int32 = record__38.(Record__int32_Pair)._1
        var after__40 int32 = x16
        return after__40, true
    case Record__int32_Empty:
        var ret_zero int32
        return ret_zero, false
    default:
        panic("non-exhaustive match")
    }
}

func gather(record__38 Record__int32) Maybe__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = gather__native(record__38)
    if native_ok {
        return Maybe__int32_Some{
            _0: native_value_0,
        }
    }
    return Maybe__int32_None{}
}

func build_counter(label__41 string, start__42 int32) Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string {
    var retv87 Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string
    var count__43 *ref_int32_x = ref__Ref_int32(start__42)
    var toggled__44 *ref_bool_x = ref__Ref_bool(false)
    var tracker__45 Tracker = Tracker{
        label: label__41,
        count: count__43,
        toggled: toggled__44,
    }
    var snapshot__46 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__43,
    }
    var bump__49 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__43,
    }
    var flip__52 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__44,
    }
    var t88 Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string = Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string{
        _0: tracker__45,
        _1: func() Record__int32 {
            return _goml_inherent_closure_env_snapshot_0_closure_env_snapshot_0_apply(snapshot__46)
        },
        _2: func(p0 int32) Record__int32 {
            return _goml_inherent_closure_env_bump_1_closure_env_bump_1_apply(bump__49, p0)
        },
        _3: func() Record__string {
            return _goml_inherent_closure_env_flip_2_closure_env_flip_2_apply(flip__52)
        },
    }
    retv87 = t88
    return retv87
}

func main0() struct{} {
    var mtmp19 Tuple4_Tracker_TFunc_unit_Record__int32_TFunc_int32_Record__int32_TFunc_unit_Record__string = build_counter("goml", 2)
    var x20 Tracker = mtmp19._0
    var x21 func() Record__int32 = mtmp19._1
    var x22 func(int32) Record__int32 = mtmp19._2
    var x23 func() Record__string = mtmp19._3
    var flip__56 func() Record__string = x23
    var bump__55 func(int32) Record__int32 = x22
    var snapshot__54 func() Record__int32 = x21
    var tracker__53 Tracker = x20
    var tracker_info__57 string = _goml_trait_impl_Describe_Tracker_describe(tracker__53)
    var first_record__58 Record__int32 = snapshot__54()
    var bumped_record__59 Record__int32 = bump__55(5)
    var flipped_record__60 Record__string = flip__56()
    var maybe_first__61 Maybe__int32 = gather(first_record__58)
    var maybe_second__62 Maybe__int32 = gather(bumped_record__59)
    var chosen__63 Maybe__int32 = _goml_choose__T_Maybe_x5b_int32_x5d_(true, maybe_second__62, maybe_first__61)
    var stringified__64 Maybe__string = map_maybe__T_int32__U_string(chosen__63, format_total)
    var transforms__65 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__66 [2]int32 = run_transforms(4, transforms__65)
    var first_result__67 int32 = array_get__Array_2_int32(results__66, 0)
    var second_result__68 int32 = array_get__Array_2_int32(results__66, 1)
    var t90 bool = first_result__67 < second_result__68
    var order_check__69 bool = t90 && true
    var first_text__70 string = _goml_trait_impl_Describe_Record__int32_describe(first_record__58)
    var bumped_text__71 string = _goml_trait_impl_Describe_Record__int32_describe(bumped_record__59)
    var flipped_text__72 string = _goml_trait_impl_Describe_Record__string_describe(flipped_record__60)
    var jp92 string
    switch stringified__64.(type) {
    case Maybe__string_Some:
        var x24 string = stringified__64.(Maybe__string_Some)._0
        var text__73 string = x24
        var t96 string = "Snapshot: " + text__73
        jp92 = t96
    case Maybe__string_None:
        jp92 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__74 string = jp92
    var t93 string = int32_to_string(first_result__67)
    var t94 string = int32_to_string(second_result__68)
    var t95 Tuple2_string_string = Tuple2_string_string{
        _0: t93,
        _1: t94,
    }
    var pair_text__75 string = pair_join(t95)
    var bool_text__76 string = bool_to_string(order_check__69)
    string_println(tracker_info__57)
    string_println(first_text__70)
    string_println(bumped_text__71)
    string_println(flipped_text__72)
    string_println(summary__74)
    string_println(pair_text__75)
    string_println(bool_text__76)
    return struct{}{}
}

func _goml_choose__T_Maybe_x5b_int32_x5d___native(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) (int32, bool) {
    if flag__20 {
        switch ret_variant := when_true__21.(type) {
        case Maybe__int32_None:
            var ret_zero int32
            return ret_zero, false
        case Maybe__int32_Some:
            return ret_variant._0, true
        default:
            panic("non-exhaustive match")
        }
    } else {
        switch ret_variant := when_false__22.(type) {
        case Maybe__int32_None:
            var ret_zero int32
            return ret_zero, false
        case Maybe__int32_Some:
            return ret_variant._0, true
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_choose__T_Maybe_x5b_int32_x5d_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = _goml_choose__T_Maybe_x5b_int32_x5d___native(flag__20, when_true__21, when_false__22)
    if native_ok {
        return Maybe__int32_Some{
            _0: native_value_0,
        }
    }
    return Maybe__int32_None{}
}

func map_maybe__T_int32__U_string__native(value__23 Maybe__int32, f__24 func(int32) string) (string, bool) {
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x10 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x10
        var t105 string = f__24(inner__25)
        return t105, true
    case Maybe__int32_None:
        var ret_zero string
        return ret_zero, false
    default:
        panic("non-exhaustive match")
    }
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var native_value_0 string
    var native_ok bool
    native_value_0, native_ok = map_maybe__T_int32__U_string__native(value__23, f__24)
    if native_ok {
        return Maybe__string_Some{
            _0: native_value_0,
        }
    }
    return Maybe__string_None{}
}

func _goml_inherent_closure_env_snapshot_0_closure_env_snapshot_0_apply(env32 closure_env_snapshot_0) Record__int32 {
    var retv108 Record__int32
    var count__43 *ref_int32_x = env32.count_0
    var t109 int32 = ref_get__Ref_int32(count__43)
    var t110 Record__int32 = Record__int32_Value{
        _0: t109,
    }
    retv108 = t110
    return retv108
}

func _goml_inherent_closure_env_bump_1_closure_env_bump_1_apply(env33 closure_env_bump_1, delta__47 int32) Record__int32 {
    var retv112 Record__int32
    var count__43 *ref_int32_x = env33.count_0
    var before__48 int32 = ref_get__Ref_int32(count__43)
    var t113 int32 = before__48 + delta__47
    ref_set__Ref_int32(count__43, t113)
    var t114 int32 = ref_get__Ref_int32(count__43)
    var t115 Record__int32 = Record__int32_Pair{
        _0: before__48,
        _1: t114,
    }
    retv112 = t115
    return retv112
}

func _goml_inherent_closure_env_flip_2_closure_env_flip_2_apply(env34 closure_env_flip_2) Record__string {
    var retv117 Record__string
    var toggled__44 *ref_bool_x = env34.toggled_0
    var before__50 bool = ref_get__Ref_bool(toggled__44)
    var t118 bool = !before__50
    ref_set__Ref_bool(toggled__44, t118)
    var after__51 bool = ref_get__Ref_bool(toggled__44)
    var t119 string = bool_to_string(before__50)
    var t120 string = bool_to_string(after__51)
    var t121 Record__string = Record__string_Pair{
        _0: t119,
        _1: t120,
    }
    retv117 = t121
    return retv117
}

func main() {
    main0()
}
