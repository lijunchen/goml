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
    var retv99 string
    var mtmp58 Tracker = self__0
    var x59 string = mtmp58.label
    var x60 *ref_int32_x = mtmp58.count
    var x61 *ref_bool_x = mtmp58.toggled
    var toggled__3 *ref_bool_x = x61
    var count__2 *ref_int32_x = x60
    var label__1 string = x59
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t100 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t100
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t101 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t102 string = with_flag_label__9 + t101
    var t103 string = t102 + ")"
    retv99 = t103
    return retv99
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv105 string
    var jp107 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x62 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x62
        var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t109 string = "Value(" + t108
        var t110 string = t109 + ")"
        jp107 = t110
    case Record__int32_Pair:
        var x63 int32 = self__10.(Record__int32_Pair)._0
        var x64 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x64
        var before__12 int32 = x63
        var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t111
        var t112 string = prefix__14 + ", "
        var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t114 string = t112 + t113
        var t115 string = t114 + ")"
        jp107 = t115
    case Record__int32_Empty:
        jp107 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv105 = jp107
    return retv105
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv117 string
    var jp119 string
    switch self__15.(type) {
    case Record__string_Value:
        var x65 string = self__15.(Record__string_Value)._0
        var text__16 string = x65
        var t120 string = "Value(" + text__16
        var t121 string = t120 + ")"
        jp119 = t121
    case Record__string_Pair:
        var x66 string = self__15.(Record__string_Pair)._0
        var x67 string = self__15.(Record__string_Pair)._1
        var after__18 string = x67
        var before__17 string = x66
        var prefix__19 string = "Pair(" + before__17
        var t122 string = prefix__19 + ", "
        var t123 string = t122 + after__18
        var t124 string = t123 + ")"
        jp119 = t124
    case Record__string_Empty:
        jp119 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv117 = jp119
    return retv117
}

func format_total(total__26 int32) string {
    var retv126 string
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t128 string = "total: " + t127
    retv126 = t128
    return retv126
}

func increment(value__27 int32) int32 {
    var retv130 int32
    var t131 int32 = value__27 + 1
    retv130 = t131
    return retv130
}

func triple(value__28 int32) int32 {
    var retv133 int32
    var t134 int32 = value__28 * 3
    retv133 = t134
    return retv133
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv136 string
    var mtmp69 Tuple2_6string_6string = parts__29
    var x70 string = mtmp69._0
    var x71 string = mtmp69._1
    var right__31 string = x71
    var left__30 string = x70
    var t137 string = left__30 + " -> "
    var t138 string = t137 + right__31
    retv136 = t138
    return retv136
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv140 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root72 [2]int32 = results__38
    var index73 int32 = 1
    array_get__Array_2_5int32(place_root72, index73)
    var value75 int32 = second_result__37
    var t141 [2]int32 = array_set__Array_2_5int32(place_root72, index73, value75)
    results__38 = t141
    retv140 = results__38
    return retv140
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv144 Maybe__int32
    var jp146 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x77 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x77
        var t147 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp146 = t147
    case Record__int32_Pair:
        var x79 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x79
        var t148 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp146 = t148
    case Record__int32_Empty:
        jp146 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv144 = jp146
    return retv144
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv150 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t151 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv150 = t151
    return retv150
}

func main0() struct{} {
    var mtmp82 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x83 Tracker = mtmp82._0
    var x84 func() Record__int32 = mtmp82._1
    var x85 func(int32) Record__int32 = mtmp82._2
    var x86 func() Record__string = mtmp82._3
    var flip__57 func() Record__string = x86
    var bump__56 func(int32) Record__int32 = x85
    var snapshot__55 func() Record__int32 = x84
    var tracker__54 Tracker = x83
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
    var t161 bool = first_result__68 < second_result__69
    var jp154 bool
    if t161 {
        jp154 = true
    } else {
        jp154 = false
    }
    var order_check__70 bool = jp154
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp156 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x87 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x87
        var t160 string = "Snapshot: " + text__74
        jp156 = t160
    case Maybe__string_None:
        jp156 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp156
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t159 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t157,
        _1: t158,
    }
    var pair_text__76 string = pair_join(t159)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv163 int32
    var t164 int32 = ref_get__Ref_5int32(self__201)
    retv163 = t164
    return retv163
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__201 *ref_bool_x) bool {
    var retv166 bool
    var t167 bool = ref_get__Ref_4bool(self__201)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int32_to_string(self__2)
    retv169 = t170
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv172 string
    var t173 string = _goml_runtime_core_bool_to_string(self__33)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv175 *ref_int32_x
    var t176 *ref_int32_x = ref__Ref_5int32(value__200)
    retv175 = t176
    return retv175
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__200 bool) *ref_bool_x {
    var retv178 *ref_bool_x
    var t179 *ref_bool_x = ref__Ref_4bool(value__200)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__202 *ref_bool_x, value__203 bool) struct{} {
    ref_set__Ref_4bool(self__202, value__203)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_int32_r_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var retv185 Maybe__int32
    var jp187 Maybe__int32
    if flag__20 {
        jp187 = when_true__21
    } else {
        jp187 = when_false__22
    }
    retv185 = jp187
    return retv185
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv189 Maybe__string
    var jp191 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x68 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x68
        var t192 string = f__24(inner__25)
        var t193 Maybe__string = Maybe__string_Some{
            _0: t192,
        }
        jp191 = t193
    case Maybe__int32_None:
        jp191 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv189 = jp191
    return retv189
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv198 string
    retv198 = self__34
    return retv198
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env95 closure_env_snapshot_0) Record__int32 {
    var retv213 Record__int32
    var count__44 *ref_int32_x = env95.count_0
    var t214 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t215 Record__int32 = Record__int32_Value{
        _0: t214,
    }
    retv213 = t215
    return retv213
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env96 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv217 Record__int32
    var count__44 *ref_int32_x = env96.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t218 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t218)
    var t219 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t220 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t219,
    }
    retv217 = t220
    return retv217
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env97 closure_env_flip_2) Record__string {
    var retv222 Record__string
    var toggled__45 *ref_bool_x = env97.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t223 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t223)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t224 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t225 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t226 Record__string = Record__string_Pair{
        _0: t224,
        _1: t225,
    }
    retv222 = t226
    return retv222
}

func main() {
    main0()
}
