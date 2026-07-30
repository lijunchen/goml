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
    var retv149 string
    var mtmp108 Tracker = self__0
    var x109 string = mtmp108.label
    var x110 *ref_int32_x = mtmp108.count
    var x111 *ref_bool_x = mtmp108.toggled
    var toggled__3 *ref_bool_x = x111
    var count__2 *ref_int32_x = x110
    var label__1 string = x109
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t150 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t150
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t151 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t152 string = with_flag_label__9 + t151
    var t153 string = t152 + ")"
    retv149 = t153
    return retv149
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv155 string
    var jp157 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x112 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x112
        var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t159 string = "Value(" + t158
        var t160 string = t159 + ")"
        jp157 = t160
    case Record__int32_Pair:
        var x113 int32 = self__10.(Record__int32_Pair)._0
        var x114 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x114
        var before__12 int32 = x113
        var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t161
        var t162 string = prefix__14 + ", "
        var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t164 string = t162 + t163
        var t165 string = t164 + ")"
        jp157 = t165
    case Record__int32_Empty:
        jp157 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv155 = jp157
    return retv155
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv167 string
    var jp169 string
    switch self__15.(type) {
    case Record__string_Value:
        var x115 string = self__15.(Record__string_Value)._0
        var text__16 string = x115
        var t170 string = "Value(" + text__16
        var t171 string = t170 + ")"
        jp169 = t171
    case Record__string_Pair:
        var x116 string = self__15.(Record__string_Pair)._0
        var x117 string = self__15.(Record__string_Pair)._1
        var after__18 string = x117
        var before__17 string = x116
        var prefix__19 string = "Pair(" + before__17
        var t172 string = prefix__19 + ", "
        var t173 string = t172 + after__18
        var t174 string = t173 + ")"
        jp169 = t174
    case Record__string_Empty:
        jp169 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv167 = jp169
    return retv167
}

func format_total(total__26 int32) string {
    var retv176 string
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t178 string = "total: " + t177
    retv176 = t178
    return retv176
}

func increment(value__27 int32) int32 {
    var retv180 int32
    var t181 int32 = value__27 + 1
    retv180 = t181
    return retv180
}

func triple(value__28 int32) int32 {
    var retv183 int32
    var t184 int32 = value__28 * 3
    retv183 = t184
    return retv183
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv186 string
    var mtmp119 Tuple2_6string_6string = parts__29
    var x120 string = mtmp119._0
    var x121 string = mtmp119._1
    var right__31 string = x121
    var left__30 string = x120
    var t187 string = left__30 + " -> "
    var t188 string = t187 + right__31
    retv186 = t188
    return retv186
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv190 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root122 [2]int32 = results__38
    var index123 int = 1
    array_get__Array_2_5int32(place_root122, index123)
    var value125 int32 = second_result__37
    var t191 [2]int32 = array_set__Array_2_5int32(place_root122, index123, value125)
    results__38 = t191
    retv190 = results__38
    return retv190
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv194 Maybe__int32
    var jp196 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x127 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x127
        var t197 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp196 = t197
    case Record__int32_Pair:
        var x129 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x129
        var t198 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp196 = t198
    case Record__int32_Empty:
        jp196 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv194 = jp196
    return retv194
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv200 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t201 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv200 = t201
    return retv200
}

func main0() struct{} {
    var mtmp132 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x133 Tracker = mtmp132._0
    var x134 func() Record__int32 = mtmp132._1
    var x135 func(int32) Record__int32 = mtmp132._2
    var x136 func() Record__string = mtmp132._3
    var flip__57 func() Record__string = x136
    var bump__56 func(int32) Record__int32 = x135
    var snapshot__55 func() Record__int32 = x134
    var tracker__54 Tracker = x133
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
    var t211 bool = first_result__68 < second_result__69
    var jp204 bool
    if t211 {
        jp204 = true
    } else {
        jp204 = false
    }
    var order_check__70 bool = jp204
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp206 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x137 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x137
        var t210 string = "Snapshot: " + text__74
        jp206 = t210
    case Maybe__string_None:
        jp206 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp206
    var t207 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t208 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t209 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t207,
        _1: t208,
    }
    var pair_text__76 string = pair_join(t209)
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
    var retv213 int32
    var t214 int32 = ref_get__Ref_5int32(self__208)
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv216 bool
    var t217 bool = ref_get__Ref_4bool(self__208)
    retv216 = t217
    return retv216
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv219 string
    var t220 string = _goml_runtime_core_int32_to_string(self__6)
    retv219 = t220
    return retv219
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv222 string
    var t223 string = _goml_runtime_core_bool_to_string(self__37)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv225 *ref_int32_x
    var t226 *ref_int32_x = ref__Ref_5int32(value__207)
    retv225 = t226
    return retv225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv228 *ref_bool_x
    var t229 *ref_bool_x = ref__Ref_4bool(value__207)
    retv228 = t229
    return retv228
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
    var retv235 Maybe__int32
    var jp237 Maybe__int32
    if flag__20 {
        jp237 = when_true__21
    } else {
        jp237 = when_false__22
    }
    retv235 = jp237
    return retv235
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv239 Maybe__string
    var jp241 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x118 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x118
        var t242 string = f__24(inner__25)
        var t243 Maybe__string = Maybe__string_Some{
            _0: t242,
        }
        jp241 = t243
    case Maybe__int32_None:
        jp241 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv239 = jp241
    return retv239
}

func println__T_string(value__1 string) struct{} {
    var t245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv248 string
    retv248 = self__38
    return retv248
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env145 closure_env_snapshot_0) Record__int32 {
    var retv263 Record__int32
    var count__44 *ref_int32_x = env145.count_0
    var t264 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t265 Record__int32 = Record__int32_Value{
        _0: t264,
    }
    retv263 = t265
    return retv263
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env146 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv267 Record__int32
    var count__44 *ref_int32_x = env146.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t268 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t268)
    var t269 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t270 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t269,
    }
    retv267 = t270
    return retv267
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env147 closure_env_flip_2) Record__string {
    var retv272 Record__string
    var toggled__45 *ref_bool_x = env147.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t273 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t273)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t274 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t275 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t276 Record__string = Record__string_Pair{
        _0: t274,
        _1: t275,
    }
    retv272 = t276
    return retv272
}

func main() {
    main0()
}
