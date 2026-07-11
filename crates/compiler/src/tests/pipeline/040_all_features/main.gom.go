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
    var retv63 string
    var mtmp22 Tracker = self__0
    var x23 string = mtmp22.label
    var x24 *ref_int32_x = mtmp22.count
    var x25 *ref_bool_x = mtmp22.toggled
    var toggled__3 *ref_bool_x = x25
    var count__2 *ref_int32_x = x24
    var label__1 string = x23
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t64
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t65 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t66 string = with_flag_label__9 + t65
    var t67 string = t66 + ")"
    retv63 = t67
    return retv63
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv69 string
    var jp71 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x26 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x26
        var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t73 string = "Value(" + t72
        var t74 string = t73 + ")"
        jp71 = t74
    case Record__int32_Pair:
        var x27 int32 = self__10.(Record__int32_Pair)._0
        var x28 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x28
        var before__12 int32 = x27
        var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t75
        var t76 string = prefix__14 + ", "
        var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t78 string = t76 + t77
        var t79 string = t78 + ")"
        jp71 = t79
    case Record__int32_Empty:
        jp71 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv81 string
    var jp83 string
    switch self__15.(type) {
    case Record__string_Value:
        var x29 string = self__15.(Record__string_Value)._0
        var text__16 string = x29
        var t84 string = "Value(" + text__16
        var t85 string = t84 + ")"
        jp83 = t85
    case Record__string_Pair:
        var x30 string = self__15.(Record__string_Pair)._0
        var x31 string = self__15.(Record__string_Pair)._1
        var after__18 string = x31
        var before__17 string = x30
        var prefix__19 string = "Pair(" + before__17
        var t86 string = prefix__19 + ", "
        var t87 string = t86 + after__18
        var t88 string = t87 + ")"
        jp83 = t88
    case Record__string_Empty:
        jp83 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func format_total(total__26 int32) string {
    var retv90 string
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t92 string = "total: " + t91
    retv90 = t92
    return retv90
}

func increment(value__27 int32) int32 {
    var retv94 int32
    var t95 int32 = value__27 + 1
    retv94 = t95
    return retv94
}

func triple(value__28 int32) int32 {
    var retv97 int32
    var t98 int32 = value__28 * 3
    retv97 = t98
    return retv97
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv100 string
    var mtmp33 Tuple2_6string_6string = parts__29
    var x34 string = mtmp33._0
    var x35 string = mtmp33._1
    var right__31 string = x35
    var left__30 string = x34
    var t101 string = left__30 + " -> "
    var t102 string = t101 + right__31
    retv100 = t102
    return retv100
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv104 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root36 [2]int32 = results__38
    var index37 int32 = 1
    array_get__Array_2_5int32(place_root36, index37)
    var value39 int32 = second_result__37
    var t105 [2]int32 = array_set__Array_2_5int32(place_root36, index37, value39)
    results__38 = t105
    retv104 = results__38
    return retv104
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv108 Maybe__int32
    var jp110 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x41 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x41
        var t111 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp110 = t111
    case Record__int32_Pair:
        var x43 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x43
        var t112 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp110 = t112
    case Record__int32_Empty:
        jp110 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv108 = jp110
    return retv108
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv114 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t115 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv114 = t115
    return retv114
}

func main0() struct{} {
    var mtmp46 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x47 Tracker = mtmp46._0
    var x48 func() Record__int32 = mtmp46._1
    var x49 func(int32) Record__int32 = mtmp46._2
    var x50 func() Record__string = mtmp46._3
    var flip__57 func() Record__string = x50
    var bump__56 func(int32) Record__int32 = x49
    var snapshot__55 func() Record__int32 = x48
    var tracker__54 Tracker = x47
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
    var t125 bool = first_result__68 < second_result__69
    var jp118 bool
    if t125 {
        jp118 = true
    } else {
        jp118 = false
    }
    var order_check__70 bool = jp118
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp120 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x51 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x51
        var t124 string = "Snapshot: " + text__74
        jp120 = t124
    case Maybe__string_None:
        jp120 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp120
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t123 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t121,
        _1: t122,
    }
    var pair_text__76 string = pair_join(t123)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv127 int32
    var t128 int32 = ref_get__Ref_5int32(self__141)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__141 *ref_bool_x) bool {
    var retv130 bool
    var t131 bool = ref_get__Ref_4bool(self__141)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv133 string
    var t134 string = _goml_runtime_core_int32_to_string(self__2)
    retv133 = t134
    return retv133
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv136 string
    var t137 string = _goml_runtime_core_bool_to_string(self__8)
    retv136 = t137
    return retv136
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv139 *ref_int32_x
    var t140 *ref_int32_x = ref__Ref_5int32(value__140)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__140 bool) *ref_bool_x {
    var retv142 *ref_bool_x
    var t143 *ref_bool_x = ref__Ref_4bool(value__140)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__142 *ref_bool_x, value__143 bool) struct{} {
    ref_set__Ref_4bool(self__142, value__143)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv149 Maybe__int32
    var jp151 Maybe__int32
    if flag__20 {
        jp151 = when_true__21
    } else {
        jp151 = when_false__22
    }
    retv149 = jp151
    return retv149
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv153 Maybe__string
    var jp155 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x32 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x32
        var t156 string = f__24(inner__25)
        var t157 Maybe__string = Maybe__string_Some{
            _0: t156,
        }
        jp155 = t157
    case Maybe__int32_None:
        jp155 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv153 = jp155
    return retv153
}

func println__T_string(value__1 string) struct{} {
    var t159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t159)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv162 string
    retv162 = self__9
    return retv162
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env59 closure_env_snapshot_0) Record__int32 {
    var retv177 Record__int32
    var count__44 *ref_int32_x = env59.count_0
    var t178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t179 Record__int32 = Record__int32_Value{
        _0: t178,
    }
    retv177 = t179
    return retv177
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env60 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv181 Record__int32
    var count__44 *ref_int32_x = env60.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t182 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t182)
    var t183 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t184 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t183,
    }
    retv181 = t184
    return retv181
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env61 closure_env_flip_2) Record__string {
    var retv186 Record__string
    var toggled__45 *ref_bool_x = env61.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t187 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t187)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t188 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t190 Record__string = Record__string_Pair{
        _0: t188,
        _1: t189,
    }
    retv186 = t190
    return retv186
}

func main() {
    main0()
}
