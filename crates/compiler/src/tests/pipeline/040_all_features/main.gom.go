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
    var retv45 string
    var mtmp4 Tracker = self__0
    var x5 string = mtmp4.label
    var x6 *ref_int32_x = mtmp4.count
    var x7 *ref_bool_x = mtmp4.toggled
    var toggled__3 *ref_bool_x = x7
    var count__2 *ref_int32_x = x6
    var label__1 string = x5
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t46
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t47 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t48 string = with_flag_label__9 + t47
    var t49 string = t48 + ")"
    retv45 = t49
    return retv45
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv51 string
    var jp53 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x8 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x8
        var t54 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t55 string = "Value(" + t54
        var t56 string = t55 + ")"
        jp53 = t56
    case Record__int32_Pair:
        var x9 int32 = self__10.(Record__int32_Pair)._0
        var x10 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x10
        var before__12 int32 = x9
        var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t57
        var t58 string = prefix__14 + ", "
        var t59 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t60 string = t58 + t59
        var t61 string = t60 + ")"
        jp53 = t61
    case Record__int32_Empty:
        jp53 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv51 = jp53
    return retv51
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv63 string
    var jp65 string
    switch self__15.(type) {
    case Record__string_Value:
        var x11 string = self__15.(Record__string_Value)._0
        var text__16 string = x11
        var t66 string = "Value(" + text__16
        var t67 string = t66 + ")"
        jp65 = t67
    case Record__string_Pair:
        var x12 string = self__15.(Record__string_Pair)._0
        var x13 string = self__15.(Record__string_Pair)._1
        var after__18 string = x13
        var before__17 string = x12
        var prefix__19 string = "Pair(" + before__17
        var t68 string = prefix__19 + ", "
        var t69 string = t68 + after__18
        var t70 string = t69 + ")"
        jp65 = t70
    case Record__string_Empty:
        jp65 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv63 = jp65
    return retv63
}

func format_total(total__26 int32) string {
    var retv72 string
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t74 string = "total: " + t73
    retv72 = t74
    return retv72
}

func increment(value__27 int32) int32 {
    var retv76 int32
    var t77 int32 = value__27 + 1
    retv76 = t77
    return retv76
}

func triple(value__28 int32) int32 {
    var retv79 int32
    var t80 int32 = value__28 * 3
    retv79 = t80
    return retv79
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv82 string
    var mtmp15 Tuple2_6string_6string = parts__29
    var x16 string = mtmp15._0
    var x17 string = mtmp15._1
    var right__31 string = x17
    var left__30 string = x16
    var t83 string = left__30 + " -> "
    var t84 string = t83 + right__31
    retv82 = t84
    return retv82
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv86 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root18 [2]int32 = results__38
    var index19 int32 = 1
    array_get__Array_2_5int32(place_root18, index19)
    var value21 int32 = second_result__37
    var t87 [2]int32 = array_set__Array_2_5int32(place_root18, index19, value21)
    results__38 = t87
    retv86 = results__38
    return retv86
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv90 Maybe__int32
    var jp92 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x23 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x23
        var t93 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp92 = t93
    case Record__int32_Pair:
        var x25 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x25
        var t94 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp92 = t94
    case Record__int32_Empty:
        jp92 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv96 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
    var count__44 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__43)
    var toggled__45 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
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
    var t97 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv96 = t97
    return retv96
}

func main0() struct{} {
    var mtmp28 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x29 Tracker = mtmp28._0
    var x30 func() Record__int32 = mtmp28._1
    var x31 func(int32) Record__int32 = mtmp28._2
    var x32 func() Record__string = mtmp28._3
    var flip__57 func() Record__string = x32
    var bump__56 func(int32) Record__int32 = x31
    var snapshot__55 func() Record__int32 = x30
    var tracker__54 Tracker = x29
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(tracker__54)
    var first_record__59 Record__int32 = snapshot__55()
    var bumped_record__60 Record__int32 = bump__56(5)
    var flipped_record__61 Record__string = flip__57()
    var maybe_first__62 Maybe__int32 = gather(first_record__59)
    var maybe_second__63 Maybe__int32 = gather(bumped_record__60)
    var chosen__64 Maybe__int32 = _goml_m_choose____T__Maybe_l_int32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_int32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32 = run_transforms(4, transforms__66)
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t107 bool = first_result__68 < second_result__69
    var jp100 bool
    if t107 {
        jp100 = true
    } else {
        jp100 = false
    }
    var order_check__70 bool = jp100
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp102 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x33 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x33
        var t106 string = "Snapshot: " + text__74
        jp102 = t106
    case Maybe__string_None:
        jp102 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp102
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t104 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t105 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t103,
        _1: t104,
    }
    var pair_text__76 string = pair_join(t105)
    var bool_text__77 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(order_check__70)
    println__T_string(tracker_info__58)
    println__T_string(first_text__71)
    println__T_string(bumped_text__72)
    println__T_string(flipped_text__73)
    println__T_string(summary__75)
    println__T_string(pair_text__76)
    println__T_string(bool_text__77)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv109 int32
    var t110 int32 = ref_get__Ref_5int32(self__103)
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__103 *ref_bool_x) bool {
    var retv112 bool
    var t113 bool = ref_get__Ref_4bool(self__103)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv115 string
    var t116 string = _goml_runtime_core_int32_to_string(self__2)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv118 string
    var t119 string = _goml_runtime_core_bool_to_string(self__8)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv121 *ref_int32_x
    var t122 *ref_int32_x = ref__Ref_5int32(value__102)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__102 bool) *ref_bool_x {
    var retv124 *ref_bool_x
    var t125 *ref_bool_x = ref__Ref_4bool(value__102)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__104 *ref_bool_x, value__105 bool) struct{} {
    ref_set__Ref_4bool(self__104, value__105)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv131 Maybe__int32
    var jp133 Maybe__int32
    if flag__20 {
        jp133 = when_true__21
    } else {
        jp133 = when_false__22
    }
    retv131 = jp133
    return retv131
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv135 Maybe__string
    var jp137 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x14 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x14
        var t138 string = f__24(inner__25)
        var t139 Maybe__string = Maybe__string_Some{
            _0: t138,
        }
        jp137 = t139
    case Maybe__int32_None:
        jp137 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv135 = jp137
    return retv135
}

func println__T_string(value__1 string) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv144 string
    retv144 = self__9
    return retv144
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env41 closure_env_snapshot_0) Record__int32 {
    var retv159 Record__int32
    var count__44 *ref_int32_x = env41.count_0
    var t160 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t161 Record__int32 = Record__int32_Value{
        _0: t160,
    }
    retv159 = t161
    return retv159
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env42 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv163 Record__int32
    var count__44 *ref_int32_x = env42.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t164 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t164)
    var t165 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t166 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t165,
    }
    retv163 = t166
    return retv163
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env43 closure_env_flip_2) Record__string {
    var retv168 Record__string
    var toggled__45 *ref_bool_x = env43.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t169 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t169)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t170 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t171 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t172 Record__string = Record__string_Pair{
        _0: t170,
        _1: t171,
    }
    retv168 = t172
    return retv168
}

func main() {
    main0()
}
