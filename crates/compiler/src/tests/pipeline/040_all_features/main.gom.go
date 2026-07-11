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
    var retv48 string
    var mtmp7 Tracker = self__0
    var x8 string = mtmp7.label
    var x9 *ref_int32_x = mtmp7.count
    var x10 *ref_bool_x = mtmp7.toggled
    var toggled__3 *ref_bool_x = x10
    var count__2 *ref_int32_x = x9
    var label__1 string = x8
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t49
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t50 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t51 string = with_flag_label__9 + t50
    var t52 string = t51 + ")"
    retv48 = t52
    return retv48
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv54 string
    var jp56 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x11 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x11
        var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t58 string = "Value(" + t57
        var t59 string = t58 + ")"
        jp56 = t59
    case Record__int32_Pair:
        var x12 int32 = self__10.(Record__int32_Pair)._0
        var x13 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x13
        var before__12 int32 = x12
        var t60 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t60
        var t61 string = prefix__14 + ", "
        var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t63 string = t61 + t62
        var t64 string = t63 + ")"
        jp56 = t64
    case Record__int32_Empty:
        jp56 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv66 string
    var jp68 string
    switch self__15.(type) {
    case Record__string_Value:
        var x14 string = self__15.(Record__string_Value)._0
        var text__16 string = x14
        var t69 string = "Value(" + text__16
        var t70 string = t69 + ")"
        jp68 = t70
    case Record__string_Pair:
        var x15 string = self__15.(Record__string_Pair)._0
        var x16 string = self__15.(Record__string_Pair)._1
        var after__18 string = x16
        var before__17 string = x15
        var prefix__19 string = "Pair(" + before__17
        var t71 string = prefix__19 + ", "
        var t72 string = t71 + after__18
        var t73 string = t72 + ")"
        jp68 = t73
    case Record__string_Empty:
        jp68 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func format_total(total__26 int32) string {
    var retv75 string
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t77 string = "total: " + t76
    retv75 = t77
    return retv75
}

func increment(value__27 int32) int32 {
    var retv79 int32
    var t80 int32 = value__27 + 1
    retv79 = t80
    return retv79
}

func triple(value__28 int32) int32 {
    var retv82 int32
    var t83 int32 = value__28 * 3
    retv82 = t83
    return retv82
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv85 string
    var mtmp18 Tuple2_6string_6string = parts__29
    var x19 string = mtmp18._0
    var x20 string = mtmp18._1
    var right__31 string = x20
    var left__30 string = x19
    var t86 string = left__30 + " -> "
    var t87 string = t86 + right__31
    retv85 = t87
    return retv85
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv89 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root21 [2]int32 = results__38
    var index22 int32 = 1
    array_get__Array_2_5int32(place_root21, index22)
    var value24 int32 = second_result__37
    var t90 [2]int32 = array_set__Array_2_5int32(place_root21, index22, value24)
    results__38 = t90
    retv89 = results__38
    return retv89
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv93 Maybe__int32
    var jp95 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x26 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x26
        var t96 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp95 = t96
    case Record__int32_Pair:
        var x28 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x28
        var t97 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp95 = t97
    case Record__int32_Empty:
        jp95 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv99 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t100 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv99 = t100
    return retv99
}

func main0() struct{} {
    var mtmp31 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x32 Tracker = mtmp31._0
    var x33 func() Record__int32 = mtmp31._1
    var x34 func(int32) Record__int32 = mtmp31._2
    var x35 func() Record__string = mtmp31._3
    var flip__57 func() Record__string = x35
    var bump__56 func(int32) Record__int32 = x34
    var snapshot__55 func() Record__int32 = x33
    var tracker__54 Tracker = x32
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
    var t110 bool = first_result__68 < second_result__69
    var jp103 bool
    if t110 {
        jp103 = true
    } else {
        jp103 = false
    }
    var order_check__70 bool = jp103
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp105 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x36 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x36
        var t109 string = "Snapshot: " + text__74
        jp105 = t109
    case Maybe__string_None:
        jp105 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp105
    var t106 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t107 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t108 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t106,
        _1: t107,
    }
    var pair_text__76 string = pair_join(t108)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv112 int32
    var t113 int32 = ref_get__Ref_5int32(self__115)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__115 *ref_bool_x) bool {
    var retv115 bool
    var t116 bool = ref_get__Ref_4bool(self__115)
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__2)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv121 string
    var t122 string = _goml_runtime_core_bool_to_string(self__8)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv124 *ref_int32_x
    var t125 *ref_int32_x = ref__Ref_5int32(value__114)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__114 bool) *ref_bool_x {
    var retv127 *ref_bool_x
    var t128 *ref_bool_x = ref__Ref_4bool(value__114)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__116 *ref_bool_x, value__117 bool) struct{} {
    ref_set__Ref_4bool(self__116, value__117)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv134 Maybe__int32
    var jp136 Maybe__int32
    if flag__20 {
        jp136 = when_true__21
    } else {
        jp136 = when_false__22
    }
    retv134 = jp136
    return retv134
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv138 Maybe__string
    var jp140 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x17 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x17
        var t141 string = f__24(inner__25)
        var t142 Maybe__string = Maybe__string_Some{
            _0: t141,
        }
        jp140 = t142
    case Maybe__int32_None:
        jp140 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv138 = jp140
    return retv138
}

func println__T_string(value__1 string) struct{} {
    var t144 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t144)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv147 string
    retv147 = self__9
    return retv147
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env44 closure_env_snapshot_0) Record__int32 {
    var retv162 Record__int32
    var count__44 *ref_int32_x = env44.count_0
    var t163 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t164 Record__int32 = Record__int32_Value{
        _0: t163,
    }
    retv162 = t164
    return retv162
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env45 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv166 Record__int32
    var count__44 *ref_int32_x = env45.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t167 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t167)
    var t168 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t169 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t168,
    }
    retv166 = t169
    return retv166
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env46 closure_env_flip_2) Record__string {
    var retv171 Record__string
    var toggled__45 *ref_bool_x = env46.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t172 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t172)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t173 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t174 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t175 Record__string = Record__string_Pair{
        _0: t173,
        _1: t174,
    }
    retv171 = t175
    return retv171
}

func main() {
    main0()
}
