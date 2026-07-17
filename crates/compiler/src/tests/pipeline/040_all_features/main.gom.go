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
    var retv102 string
    var mtmp61 Tracker = self__0
    var x62 string = mtmp61.label
    var x63 *ref_int32_x = mtmp61.count
    var x64 *ref_bool_x = mtmp61.toggled
    var toggled__3 *ref_bool_x = x64
    var count__2 *ref_int32_x = x63
    var label__1 string = x62
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t103
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t104 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t105 string = with_flag_label__9 + t104
    var t106 string = t105 + ")"
    retv102 = t106
    return retv102
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv108 string
    var jp110 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x65 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x65
        var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t112 string = "Value(" + t111
        var t113 string = t112 + ")"
        jp110 = t113
    case Record__int32_Pair:
        var x66 int32 = self__10.(Record__int32_Pair)._0
        var x67 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x67
        var before__12 int32 = x66
        var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t114
        var t115 string = prefix__14 + ", "
        var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t117 string = t115 + t116
        var t118 string = t117 + ")"
        jp110 = t118
    case Record__int32_Empty:
        jp110 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv108 = jp110
    return retv108
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv120 string
    var jp122 string
    switch self__15.(type) {
    case Record__string_Value:
        var x68 string = self__15.(Record__string_Value)._0
        var text__16 string = x68
        var t123 string = "Value(" + text__16
        var t124 string = t123 + ")"
        jp122 = t124
    case Record__string_Pair:
        var x69 string = self__15.(Record__string_Pair)._0
        var x70 string = self__15.(Record__string_Pair)._1
        var after__18 string = x70
        var before__17 string = x69
        var prefix__19 string = "Pair(" + before__17
        var t125 string = prefix__19 + ", "
        var t126 string = t125 + after__18
        var t127 string = t126 + ")"
        jp122 = t127
    case Record__string_Empty:
        jp122 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv120 = jp122
    return retv120
}

func format_total(total__26 int32) string {
    var retv129 string
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t131 string = "total: " + t130
    retv129 = t131
    return retv129
}

func increment(value__27 int32) int32 {
    var retv133 int32
    var t134 int32 = value__27 + 1
    retv133 = t134
    return retv133
}

func triple(value__28 int32) int32 {
    var retv136 int32
    var t137 int32 = value__28 * 3
    retv136 = t137
    return retv136
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv139 string
    var mtmp72 Tuple2_6string_6string = parts__29
    var x73 string = mtmp72._0
    var x74 string = mtmp72._1
    var right__31 string = x74
    var left__30 string = x73
    var t140 string = left__30 + " -> "
    var t141 string = t140 + right__31
    retv139 = t141
    return retv139
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv143 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root75 [2]int32 = results__38
    var index76 int32 = 1
    array_get__Array_2_5int32(place_root75, index76)
    var value78 int32 = second_result__37
    var t144 [2]int32 = array_set__Array_2_5int32(place_root75, index76, value78)
    results__38 = t144
    retv143 = results__38
    return retv143
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv147 Maybe__int32
    var jp149 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x80 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x80
        var t150 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp149 = t150
    case Record__int32_Pair:
        var x82 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x82
        var t151 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp149 = t151
    case Record__int32_Empty:
        jp149 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv147 = jp149
    return retv147
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv153 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t154 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv153 = t154
    return retv153
}

func main0() struct{} {
    var mtmp85 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x86 Tracker = mtmp85._0
    var x87 func() Record__int32 = mtmp85._1
    var x88 func(int32) Record__int32 = mtmp85._2
    var x89 func() Record__string = mtmp85._3
    var flip__57 func() Record__string = x89
    var bump__56 func(int32) Record__int32 = x88
    var snapshot__55 func() Record__int32 = x87
    var tracker__54 Tracker = x86
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
    var t164 bool = first_result__68 < second_result__69
    var jp157 bool
    if t164 {
        jp157 = true
    } else {
        jp157 = false
    }
    var order_check__70 bool = jp157
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp159 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x90 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x90
        var t163 string = "Snapshot: " + text__74
        jp159 = t163
    case Maybe__string_None:
        jp159 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp159
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t162 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t160,
        _1: t161,
    }
    var pair_text__76 string = pair_join(t162)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv166 int32
    var t167 int32 = ref_get__Ref_5int32(self__205)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv169 bool
    var t170 bool = ref_get__Ref_4bool(self__205)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv172 string
    var t173 string = _goml_runtime_core_int32_to_string(self__5)
    retv172 = t173
    return retv172
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv175 string
    var t176 string = _goml_runtime_core_bool_to_string(self__36)
    retv175 = t176
    return retv175
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv178 *ref_int32_x
    var t179 *ref_int32_x = ref__Ref_5int32(value__204)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv181 *ref_bool_x
    var t182 *ref_bool_x = ref__Ref_4bool(value__204)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv188 Maybe__int32
    var jp190 Maybe__int32
    if flag__20 {
        jp190 = when_true__21
    } else {
        jp190 = when_false__22
    }
    retv188 = jp190
    return retv188
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv192 Maybe__string
    var jp194 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x71 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x71
        var t195 string = f__24(inner__25)
        var t196 Maybe__string = Maybe__string_Some{
            _0: t195,
        }
        jp194 = t196
    case Maybe__int32_None:
        jp194 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv192 = jp194
    return retv192
}

func println__T_string(value__1 string) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv201 string
    retv201 = self__37
    return retv201
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env98 closure_env_snapshot_0) Record__int32 {
    var retv216 Record__int32
    var count__44 *ref_int32_x = env98.count_0
    var t217 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t218 Record__int32 = Record__int32_Value{
        _0: t217,
    }
    retv216 = t218
    return retv216
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env99 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv220 Record__int32
    var count__44 *ref_int32_x = env99.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t221 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t221)
    var t222 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t223 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t222,
    }
    retv220 = t223
    return retv220
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env100 closure_env_flip_2) Record__string {
    var retv225 Record__string
    var toggled__45 *ref_bool_x = env100.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t226 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t226)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t227 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t228 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t229 Record__string = Record__string_Pair{
        _0: t227,
        _1: t228,
    }
    retv225 = t229
    return retv225
}

func main() {
    main0()
}
