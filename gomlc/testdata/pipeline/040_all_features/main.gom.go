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
    var retv193 string
    var mtmp152 Tracker = self__0
    var x153 string = mtmp152.label
    var x154 *ref_int32_x = mtmp152.count
    var x155 *ref_bool_x = mtmp152.toggled
    var toggled__3 *ref_bool_x = x155
    var count__2 *ref_int32_x = x154
    var label__1 string = x153
    var current__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__2)
    var flag__5 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t194 string = _goml_m_inherent_i_int32_i_int32_i_to__string(current__4)
    var with_count__8 string = with_count_label__7 + t194
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t195 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(flag__5)
    var t196 string = with_flag_label__9 + t195
    var t197 string = t196 + ")"
    retv193 = t197
    return retv193
}

func _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(self__10 Record__int32) string {
    var retv199 string
    var jp201 string
    switch self__10.(type) {
    case Record__int32_Value:
        var x156 int32 = self__10.(Record__int32_Value)._0
        var value__11 int32 = x156
        var t202 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
        var t203 string = "Value(" + t202
        var t204 string = t203 + ")"
        jp201 = t204
    case Record__int32_Pair:
        var x157 int32 = self__10.(Record__int32_Pair)._0
        var x158 int32 = self__10.(Record__int32_Pair)._1
        var after__13 int32 = x158
        var before__12 int32 = x157
        var t205 string = _goml_m_inherent_i_int32_i_int32_i_to__string(before__12)
        var prefix__14 string = "Pair(" + t205
        var t206 string = prefix__14 + ", "
        var t207 string = _goml_m_inherent_i_int32_i_int32_i_to__string(after__13)
        var t208 string = t206 + t207
        var t209 string = t208 + ")"
        jp201 = t209
    case Record__int32_Empty:
        jp201 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv199 = jp201
    return retv199
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    var retv211 string
    var jp213 string
    switch self__15.(type) {
    case Record__string_Value:
        var x159 string = self__15.(Record__string_Value)._0
        var text__16 string = x159
        var t214 string = "Value(" + text__16
        var t215 string = t214 + ")"
        jp213 = t215
    case Record__string_Pair:
        var x160 string = self__15.(Record__string_Pair)._0
        var x161 string = self__15.(Record__string_Pair)._1
        var after__18 string = x161
        var before__17 string = x160
        var prefix__19 string = "Pair(" + before__17
        var t216 string = prefix__19 + ", "
        var t217 string = t216 + after__18
        var t218 string = t217 + ")"
        jp213 = t218
    case Record__string_Empty:
        jp213 = "Empty"
    default:
        panic("non-exhaustive match")
    }
    retv211 = jp213
    return retv211
}

func format_total(total__26 int32) string {
    var retv220 string
    var t221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(total__26)
    var t222 string = "total: " + t221
    retv220 = t222
    return retv220
}

func increment(value__27 int32) int32 {
    var retv224 int32
    var t225 int32 = value__27 + 1
    retv224 = t225
    return retv224
}

func triple(value__28 int32) int32 {
    var retv227 int32
    var t228 int32 = value__28 * 3
    retv227 = t228
    return retv227
}

func pair_join(parts__29 Tuple2_6string_6string) string {
    var retv230 string
    var mtmp163 Tuple2_6string_6string = parts__29
    var x164 string = mtmp163._0
    var x165 string = mtmp163._1
    var right__31 string = x165
    var left__30 string = x164
    var t231 string = left__30 + " -> "
    var t232 string = t231 + right__31
    retv230 = t232
    return retv230
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var retv234 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var results__38 [2]int32 = [2]int32{first_result__36, value__32}
    var place_root166 [2]int32 = results__38
    var index167 int = 1
    array_get__Array_2_5int32(place_root166, index167)
    var value169 int32 = second_result__37
    var t235 [2]int32 = array_set__Array_2_5int32(place_root166, index167, value169)
    results__38 = t235
    retv234 = results__38
    return retv234
}

func gather(record__39 Record__int32) Maybe__int32 {
    var retv238 Maybe__int32
    var jp240 Maybe__int32
    switch record__39.(type) {
    case Record__int32_Value:
        var x171 int32 = record__39.(Record__int32_Value)._0
        var value__40 int32 = x171
        var t241 Maybe__int32 = Maybe__int32_Some{
            _0: value__40,
        }
        jp240 = t241
    case Record__int32_Pair:
        var x173 int32 = record__39.(Record__int32_Pair)._1
        var after__41 int32 = x173
        var t242 Maybe__int32 = Maybe__int32_Some{
            _0: after__41,
        }
        jp240 = t242
    case Record__int32_Empty:
        jp240 = Maybe__int32_None{}
    default:
        panic("non-exhaustive match")
    }
    retv238 = jp240
    return retv238
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string {
    var retv244 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string
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
    var t245 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string{
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
    retv244 = t245
    return retv244
}

func main0() struct{} {
    var mtmp176 Tuple4_7Tracker_26TFunc0_ret_1_h3f520d7da3936df91f1111a1cbf9816b_4Record__string = build_counter("goml", 2)
    var x177 Tracker = mtmp176._0
    var x178 func() Record__int32 = mtmp176._1
    var x179 func(int32) Record__int32 = mtmp176._2
    var x180 func() Record__string = mtmp176._3
    var flip__57 func() Record__string = x180
    var bump__56 func(int32) Record__int32 = x179
    var snapshot__55 func() Record__int32 = x178
    var tracker__54 Tracker = x177
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
    var t255 bool = first_result__68 < second_result__69
    var jp248 bool
    if t255 {
        jp248 = true
    } else {
        jp248 = false
    }
    var order_check__70 bool = jp248
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____int32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp250 string
    switch stringified__65.(type) {
    case Maybe__string_Some:
        var x181 string = stringified__65.(Maybe__string_Some)._0
        var text__74 string = x181
        var t254 string = "Snapshot: " + text__74
        jp250 = t254
    case Maybe__string_None:
        jp250 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var summary__75 string = jp250
    var t251 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first_result__68)
    var t252 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second_result__69)
    var t253 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t251,
        _1: t252,
    }
    var pair_text__76 string = pair_join(t253)
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
    var retv257 int32
    var t258 int32 = ref_get__Ref_5int32(self__208)
    retv257 = t258
    return retv257
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv260 bool
    var t261 bool = ref_get__Ref_4bool(self__208)
    retv260 = t261
    return retv260
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv263 string
    var t264 string = _goml_runtime_core_int32_to_string(self__6)
    retv263 = t264
    return retv263
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv266 string
    var t267 string = _goml_runtime_core_bool_to_string(self__37)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv269 *ref_int32_x
    var t270 *ref_int32_x = ref__Ref_5int32(value__207)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv272 *ref_bool_x
    var t273 *ref_bool_x = ref__Ref_4bool(value__207)
    retv272 = t273
    return retv272
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
    var retv279 Maybe__int32
    var jp281 Maybe__int32
    if flag__20 {
        jp281 = when_true__21
    } else {
        jp281 = when_false__22
    }
    retv279 = jp281
    return retv279
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var retv283 Maybe__string
    var jp285 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        var x162 int32 = value__23.(Maybe__int32_Some)._0
        var inner__25 int32 = x162
        var t286 string = f__24(inner__25)
        var t287 Maybe__string = Maybe__string_Some{
            _0: t286,
        }
        jp285 = t287
    case Maybe__int32_None:
        jp285 = Maybe__string_None{}
    default:
        panic("non-exhaustive match")
    }
    retv283 = jp285
    return retv283
}

func println__T_string(value__1 string) struct{} {
    var t289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t289)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv292 string
    retv292 = self__38
    return retv292
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env189 closure_env_snapshot_0) Record__int32 {
    var retv307 Record__int32
    var count__44 *ref_int32_x = env189.count_0
    var t308 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t309 Record__int32 = Record__int32_Value{
        _0: t308,
    }
    retv307 = t309
    return retv307
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env190 closure_env_bump_1, delta__48 int32) Record__int32 {
    var retv311 Record__int32
    var count__44 *ref_int32_x = env190.count_0
    var before__49 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t312 int32 = before__49 + delta__48
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(count__44, t312)
    var t313 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(count__44)
    var t314 Record__int32 = Record__int32_Pair{
        _0: before__49,
        _1: t313,
    }
    retv311 = t314
    return retv311
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env191 closure_env_flip_2) Record__string {
    var retv316 Record__string
    var toggled__45 *ref_bool_x = env191.toggled_0
    var before__51 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t317 bool = !before__51
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(toggled__45, t317)
    var after__52 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(toggled__45)
    var t318 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(before__51)
    var t319 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(after__52)
    var t320 Record__string = Record__string_Pair{
        _0: t318,
        _1: t319,
    }
    retv316 = t320
    return retv316
}

func main() {
    main0()
}
