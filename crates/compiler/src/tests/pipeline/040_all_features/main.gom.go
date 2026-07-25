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
    var retv105 string
    var mtmp64 Tracker = self__0
    var x65 string = mtmp64.label
    var x66 *ref_int32_x = mtmp64.count
    var x67 *ref_bool_x = mtmp64.toggled
    var toggled__3 *ref_bool_x = x67
    var count__2 *ref_int32_x = x66
    var label__1 string = x65
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t106 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t106
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t107 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t108 string = with_flag_label__9 + t107
    var t109 string = t108 + ")"
    retv105 = t109
    return retv105
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv111 string
    var jp113 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x68 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x68
        var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t115 string = "Value(" + t114
        var t116 string = t115 + ")"
        jp113 = t116
    case Record__int32_Pair:
        var x69 int32 = self__10.(Record__int32_Pair)._0
        var x70 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x70
        var before__12 int32 = x69
        var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t117
        var t118 string = prefix__14 + ", "
        var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t120 string = t118 + t119
        var t121 string = t120 + ")"
        jp113 = t121
    case Record__int32_Empty:
        jp113 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv111 = jp113
    return retv111
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv123 string
    var jp125 string
    switch self__15.(type) {
    case Record__string_Value:
        var x71 string = self__15.(Record__string_Value)._0
        var text__16 string = x71
        var t126 string = "Value(" + text__16
        var t127 string = t126 + ")"
        jp125 = t127
    case Record__string_Pair:
        var x72 string = self__15.(Record__string_Pair)._0
        var x73 string = self__15.(Record__string_Pair)._1
        var after__18 string = x73
        var before__17 string = x72
        var prefix__19 string = "Pair(" + before__17
        var t128 string = prefix__19 + ", "
        var t129 string = t128 + after__18
        var t130 string = t129 + ")"
        jp125 = t130
    case Record__string_Empty:
        jp125 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func format_total(total__26 int32) string {
    var retv132 string
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t134 string = "total: " + t133
    retv132 = t134
    return retv132
}

func increment(value__27 int32) int32 {
    var retv136 int32
    var t137 int32 = value__27 + 1
    retv136 = t137
    return retv136
}

func triple(value__28 int32) int32 {
    var retv139 int32
    var t140 int32 = value__28 * 3
    retv139 = t140
    return retv139
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv142 string
    var mtmp75 Tuple2_6string_6string = parts__29
    var x76 string = mtmp75._0
    var x77 string = mtmp75._1
    var right__31 string = x77
    var left__30 string = x76
    var t143 string = left__30 + " -> "
    var t144 string = t143 + right__31
    retv142 = t144
    return retv142
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv146 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root78 [2]int32 = results__38
    var index79 int = 1
    array_get__Array_2_5int32(place_root78, index79)
    var value81 int32 = second_result__37
    var t147 [2]int32 = array_set__Array_2_5int32(place_root78, index79, value81)
    results__38 = t147
    retv146 = results__38
    return retv146
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv150 Maybe__int32
    var jp152 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x83 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x83
        var t153 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp152 = t153
    case Record__int32_Pair:
        var x85 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x85
        var t154 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp152 = t154
    case Record__int32_Empty:
        jp152 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv150 = jp152
    return retv150
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv156 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t157 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv156 = t157
    return retv156
}

func main0() struct{} {
    var mtmp88 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x89 Tracker = mtmp88._0
    var x90 func() Record__int32 = mtmp88._1
    var x91 func(int32) Record__int32 = mtmp88._2
    var x92 func() Record__string = mtmp88._3
    var flip__57 func() Record__string = x92
    var bump__56 func(int32) Record__int32 = x91
    var snapshot__55 func() Record__int32 = x90
    var tracker__54 Tracker = x89
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
    var t167 bool = first_result__68 < second_result__69
    var jp160 bool
    if t167 {
        jp160 = true
    } else {
        jp160 = false
    }
    var order_check__70 bool = jp160
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp162 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x93 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x93
        var t166 string = "Snapshot: " + text__74
        jp162 = t166
    case Maybe__string_None:
        jp162 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp162
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t165 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t163,
        _1: t164,
    }
    var pair_text__76 string = pair_join(t165)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv169 int32
    var t170 int32 = ref_get__Ref_5int32(self__210)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv172 bool
    var t173 bool = ref_get__Ref_4bool(self__210)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__6)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv178 string
    var t179 string = _goml_runtime_core_bool_to_string(self__37)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv181 *ref_int32_x
    var t182 *ref_int32_x = ref__Ref_5int32(value__209)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv184 *ref_bool_x
    var t185 *ref_bool_x = ref__Ref_4bool(value__209)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv191 Maybe__int32
    var jp193 Maybe__int32
    if flag__20 {
        jp193 = when_true__21
    } else {
        jp193 = when_false__22
    }
    retv191 = jp193
    return retv191
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv195 Maybe__string
    var jp197 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x74 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x74
        var t198 string = f__24(inner__25)
        var t199 Maybe__string = Maybe__string_Some{
            _0: t198,
        }
        jp197 = t199
    case Maybe__int32_None:
        jp197 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv195 = jp197
    return retv195
}

func println__T_string(value__1 string) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv204 string
    retv204 = self__38
    return retv204
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env101 closure_env_snapshot_0) Record__int32 {
    var retv219 Record__int32
    var count__44 *ref_int32_x = env101.count_0
    var t220 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t221 Record__int32 = Record__int32_Value{
        _0: t220,
    }
    retv219 = t221
    return retv219
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env102 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv223 Record__int32
    var count__44 *ref_int32_x = env102.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t224 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t224)
    var t225 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t226 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t225,
    }
    retv223 = t226
    return retv223
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env103 closure_env_flip_2) Record__string {
    var retv228 Record__string
    var toggled__45 *ref_bool_x = env103.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t229 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t229)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t230 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t231 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t232 Record__string = Record__string_Pair{
        _0: t230,
        _1: t231,
    }
    retv228 = t232
    return retv228
}

func main() {
    main0()
}
