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
    var retv109 string
    var mtmp68 Tracker = self__0
    var x69 string = mtmp68.label
    var x70 *ref_int32_x = mtmp68.count
    var x71 *ref_bool_x = mtmp68.toggled
    var toggled__3 *ref_bool_x = x71
    var count__2 *ref_int32_x = x70
    var label__1 string = x69
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t110
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t111 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t112 string = with_flag_label__9 + t111
    var t113 string = t112 + ")"
    retv109 = t113
    return retv109
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv115 string
    var jp117 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x72 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x72
        var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t119 string = "Value(" + t118
        var t120 string = t119 + ")"
        jp117 = t120
    case Record__int32_Pair:
        var x73 int32 = self__10.(Record__int32_Pair)._0
        var x74 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x74
        var before__12 int32 = x73
        var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t121
        var t122 string = prefix__14 + ", "
        var t123 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t124 string = t122 + t123
        var t125 string = t124 + ")"
        jp117 = t125
    case Record__int32_Empty:
        jp117 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv115 = jp117
    return retv115
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv127 string
    var jp129 string
    switch self__15.(type) {
    case Record__string_Value:
        var x75 string = self__15.(Record__string_Value)._0
        var text__16 string = x75
        var t130 string = "Value(" + text__16
        var t131 string = t130 + ")"
        jp129 = t131
    case Record__string_Pair:
        var x76 string = self__15.(Record__string_Pair)._0
        var x77 string = self__15.(Record__string_Pair)._1
        var after__18 string = x77
        var before__17 string = x76
        var prefix__19 string = "Pair(" + before__17
        var t132 string = prefix__19 + ", "
        var t133 string = t132 + after__18
        var t134 string = t133 + ")"
        jp129 = t134
    case Record__string_Empty:
        jp129 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv127 = jp129
    return retv127
}

func format_total(total__26 int32) string {
    var retv136 string
    var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t138 string = "total: " + t137
    retv136 = t138
    return retv136
}

func increment(value__27 int32) int32 {
    var retv140 int32
    var t141 int32 = value__27 + 1
    retv140 = t141
    return retv140
}

func triple(value__28 int32) int32 {
    var retv143 int32
    var t144 int32 = value__28 * 3
    retv143 = t144
    return retv143
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv146 string
    var mtmp79 Tuple2_6string_6string = parts__29
    var x80 string = mtmp79._0
    var x81 string = mtmp79._1
    var right__31 string = x81
    var left__30 string = x80
    var t147 string = left__30 + " -> "
    var t148 string = t147 + right__31
    retv146 = t148
    return retv146
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv150 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root82 [2]int32 = results__38
    var index83 int = 1
    array_get__Array_2_5int32(place_root82, index83)
    var value85 int32 = second_result__37
    var t151 [2]int32 = array_set__Array_2_5int32(place_root82, index83, value85)
    results__38 = t151
    retv150 = results__38
    return retv150
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv154 Maybe__int32
    var jp156 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x87 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x87
        var t157 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp156 = t157
    case Record__int32_Pair:
        var x89 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x89
        var t158 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp156 = t158
    case Record__int32_Empty:
        jp156 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv154 = jp156
    return retv154
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv160 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t161 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv160 = t161
    return retv160
}

func main0() struct{} {
    var mtmp92 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x93 Tracker = mtmp92._0
    var x94 func() Record__int32 = mtmp92._1
    var x95 func(int32) Record__int32 = mtmp92._2
    var x96 func() Record__string = mtmp92._3
    var flip__57 func() Record__string = x96
    var bump__56 func(int32) Record__int32 = x95
    var snapshot__55 func() Record__int32 = x94
    var tracker__54 Tracker = x93
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
    var t171 bool = first_result__68 < second_result__69
    var jp164 bool
    if t171 {
        jp164 = true
    } else {
        jp164 = false
    }
    var order_check__70 bool = jp164
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp166 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x97 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x97
        var t170 string = "Snapshot: " + text__74
        jp166 = t170
    case Maybe__string_None:
        jp166 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp166
    var t167 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t169 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t167,
        _1: t168,
    }
    var pair_text__76 string = pair_join(t169)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv173 int32
    var t174 int32 = ref_get__Ref_5int32(self__208)
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv176 bool
    var t177 bool = ref_get__Ref_4bool(self__208)
    retv176 = t177
    return retv176
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv179 string
    var t180 string = _goml_runtime_core_int32_to_string(self__6)
    retv179 = t180
    return retv179
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv182 string
    var t183 string = _goml_runtime_core_bool_to_string(self__37)
    retv182 = t183
    return retv182
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv185 *ref_int32_x
    var t186 *ref_int32_x = ref__Ref_5int32(value__207)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv188 *ref_bool_x
    var t189 *ref_bool_x = ref__Ref_4bool(value__207)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv195 Maybe__int32
    var jp197 Maybe__int32
    if flag__20 {
        jp197 = when_true__21
    } else {
        jp197 = when_false__22
    }
    retv195 = jp197
    return retv195
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv199 Maybe__string
    var jp201 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x78 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x78
        var t202 string = f__24(inner__25)
        var t203 Maybe__string = Maybe__string_Some{
            _0: t202,
        }
        jp201 = t203
    case Maybe__int32_None:
        jp201 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv199 = jp201
    return retv199
}

func println__T_string(value__1 string) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv208 string
    retv208 = self__38
    return retv208
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env105 closure_env_snapshot_0) Record__int32 {
    var retv223 Record__int32
    var count__44 *ref_int32_x = env105.count_0
    var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t225 Record__int32 = Record__int32_Value{
        _0: t224,
    }
    retv223 = t225
    return retv223
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env106 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv227 Record__int32
    var count__44 *ref_int32_x = env106.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t228 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t228)
    var t229 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t230 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t229,
    }
    retv227 = t230
    return retv227
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env107 closure_env_flip_2) Record__string {
    var retv232 Record__string
    var toggled__45 *ref_bool_x = env107.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t233 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t233)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t234 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t235 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t236 Record__string = Record__string_Pair{
        _0: t234,
        _1: t235,
    }
    retv232 = t236
    return retv232
}

func main() {
    main0()
}
