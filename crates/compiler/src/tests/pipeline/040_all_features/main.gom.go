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

type Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2 struct {
    _0 Tracker
    _1 closure_env_snapshot_0
    _2 closure_env_bump_1
    _3 closure_env_flip_2
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

func _goml_trait_impl_Describe_Tracker_describe(self__0 Tracker) string {
    var mtmp0 Tracker
    var x1 string
    var x2 *ref_int32_x
    var x3 *ref_bool_x
    var toggled__3 *ref_bool_x
    var count__2 *ref_int32_x
    var label__1 string
    var current__4 int32
    var flag__5 bool
    var with_label__6 string
    var with_count_label__7 string
    var t35 string
    var with_count__8 string
    var with_flag_label__9 string
    var t36 string
    var t37 string
    var t38 string
    mtmp0 = self__0
    x1 = mtmp0.label
    x2 = mtmp0.count
    x3 = mtmp0.toggled
    toggled__3 = x3
    count__2 = x2
    label__1 = x1
    current__4 = ref_get__Ref_int32(count__2)
    flag__5 = ref_get__Ref_bool(toggled__3)
    with_label__6 = "Tracker(" + label__1
    with_count_label__7 = with_label__6 + ", count: "
    t35 = int32_to_string(current__4)
    with_count__8 = with_count_label__7 + t35
    with_flag_label__9 = with_count__8 + ", toggled: "
    t36 = bool_to_string(flag__5)
    t37 = with_flag_label__9 + t36
    t38 = t37 + ")"
    return t38
}

func _goml_trait_impl_Describe_Record_x5b_int32_x5d__describe(self__10 Record__int32) string {
    var jp40 string
    var x4 int32
    var value__11 int32
    var t41 string
    var t42 string
    var t43 string
    var x5 int32
    var x6 int32
    var after__13 int32
    var before__12 int32
    var t44 string
    var prefix__14 string
    var t45 string
    var t46 string
    var t47 string
    var t48 string
    switch self__10.(type) {
    case Record__int32_Value:
        goto b2
    case Record__int32_Pair:
        goto b3
    case Record__int32_Empty:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp40
    b2:
    x4 = self__10.(Record__int32_Value)._0
    value__11 = x4
    t41 = int32_to_string(value__11)
    t42 = "Value(" + t41
    t43 = t42 + ")"
    jp40 = t43
    goto b1
    b3:
    x5 = self__10.(Record__int32_Pair)._0
    x6 = self__10.(Record__int32_Pair)._1
    after__13 = x6
    before__12 = x5
    t44 = int32_to_string(before__12)
    prefix__14 = "Pair(" + t44
    t45 = prefix__14 + ", "
    t46 = int32_to_string(after__13)
    t47 = t45 + t46
    t48 = t47 + ")"
    jp40 = t48
    goto b1
    b4:
    jp40 = "Empty"
    goto b1
}

func _goml_trait_impl_Describe_Record_x5b_string_x5d__describe(self__15 Record__string) string {
    var jp50 string
    var x7 string
    var text__16 string
    var t51 string
    var t52 string
    var x8 string
    var x9 string
    var after__18 string
    var before__17 string
    var prefix__19 string
    var t53 string
    var t54 string
    var t55 string
    switch self__15.(type) {
    case Record__string_Value:
        goto b2
    case Record__string_Pair:
        goto b3
    case Record__string_Empty:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp50
    b2:
    x7 = self__15.(Record__string_Value)._0
    text__16 = x7
    t51 = "Value(" + text__16
    t52 = t51 + ")"
    jp50 = t52
    goto b1
    b3:
    x8 = self__15.(Record__string_Pair)._0
    x9 = self__15.(Record__string_Pair)._1
    after__18 = x9
    before__17 = x8
    prefix__19 = "Pair(" + before__17
    t53 = prefix__19 + ", "
    t54 = t53 + after__18
    t55 = t54 + ")"
    jp50 = t55
    goto b1
    b4:
    jp50 = "Empty"
    goto b1
}

func format_total(total__26 int32) string {
    var t56 string
    var t57 string
    t56 = int32_to_string(total__26)
    t57 = "total: " + t56
    return t57
}

func increment(value__27 int32) int32 {
    var t58 int32
    t58 = value__27 + 1
    return t58
}

func triple(value__28 int32) int32 {
    var t59 int32
    t59 = value__28 * 3
    return t59
}

func pair_join(parts__29 Tuple2_string_string) string {
    var mtmp11 Tuple2_string_string
    var x12 string
    var x13 string
    var right__31 string
    var left__30 string
    var t60 string
    var t61 string
    mtmp11 = parts__29
    x12 = mtmp11._0
    x13 = mtmp11._1
    right__31 = x13
    left__30 = x12
    t60 = left__30 + " -> "
    t61 = t60 + right__31
    return t61
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var first__34 func(int32) int32
    var second__35 func(int32) int32
    var first_result__36 int32
    var second_result__37 int32
    var t62 [2]int32
    var t63 [2]int32
    first__34 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 0)
    second__35 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 1)
    first_result__36 = first__34(value__32)
    second_result__37 = second__35(first_result__36)
    t62 = [2]int32{first_result__36, value__32}
    t63 = array_set__Array_2_int32(t62, 1, second_result__37)
    return t63
}

func gather(record__38 Record__int32) Maybe__int32 {
    var jp65 Maybe__int32
    var x14 int32
    var value__39 int32
    var t66 Maybe__int32
    var x16 int32
    var after__40 int32
    var t67 Maybe__int32
    switch record__38.(type) {
    case Record__int32_Value:
        goto b2
    case Record__int32_Pair:
        goto b3
    case Record__int32_Empty:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp65
    b2:
    x14 = record__38.(Record__int32_Value)._0
    value__39 = x14
    t66 = Maybe__int32_Some{
        _0: value__39,
    }
    jp65 = t66
    goto b1
    b3:
    x16 = record__38.(Record__int32_Pair)._1
    after__40 = x16
    t67 = Maybe__int32_Some{
        _0: after__40,
    }
    jp65 = t67
    goto b1
    b4:
    jp65 = Maybe__int32_None{}
    goto b1
}

func build_counter(label__41 string, start__42 int32) Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2 {
    var count__43 *ref_int32_x
    var toggled__44 *ref_bool_x
    var tracker__45 Tracker
    var snapshot__46 closure_env_snapshot_0
    var bump__49 closure_env_bump_1
    var flip__52 closure_env_flip_2
    var t68 Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2
    count__43 = ref__Ref_int32(start__42)
    toggled__44 = ref__Ref_bool(false)
    tracker__45 = Tracker{
        label: label__41,
        count: count__43,
        toggled: toggled__44,
    }
    snapshot__46 = closure_env_snapshot_0{
        count_0: count__43,
    }
    bump__49 = closure_env_bump_1{
        count_0: count__43,
    }
    flip__52 = closure_env_flip_2{
        toggled_0: toggled__44,
    }
    t68 = Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2{
        _0: tracker__45,
        _1: snapshot__46,
        _2: bump__49,
        _3: flip__52,
    }
    return t68
}

func main0() struct{} {
    var mtmp19 Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2
    var x20 Tracker
    var x21 closure_env_snapshot_0
    var x22 closure_env_bump_1
    var x23 closure_env_flip_2
    var flip__56 closure_env_flip_2
    var bump__55 closure_env_bump_1
    var snapshot__54 closure_env_snapshot_0
    var tracker__53 Tracker
    var tracker_info__57 string
    var first_record__58 Record__int32
    var bumped_record__59 Record__int32
    var flipped_record__60 Record__string
    var maybe_first__61 Maybe__int32
    var maybe_second__62 Maybe__int32
    var chosen__63 Maybe__int32
    var stringified__64 Maybe__string
    var transforms__65 [2]func(int32) int32
    var results__66 [2]int32
    var first_result__67 int32
    var second_result__68 int32
    var t69 bool
    var order_check__69 bool
    var first_text__70 string
    var bumped_text__71 string
    var flipped_text__72 string
    var jp71 string
    var summary__74 string
    var t72 string
    var t73 string
    var t74 Tuple2_string_string
    var pair_text__75 string
    var bool_text__76 string
    var x24 string
    var text__73 string
    var t75 string
    mtmp19 = build_counter("goml", 2)
    x20 = mtmp19._0
    x21 = mtmp19._1
    x22 = mtmp19._2
    x23 = mtmp19._3
    flip__56 = x23
    bump__55 = x22
    snapshot__54 = x21
    tracker__53 = x20
    tracker_info__57 = _goml_trait_impl_Describe_Tracker_describe(tracker__53)
    first_record__58 = _goml_inherent_closure_env_snapshot_0_closure_env_snapshot_0_apply(snapshot__54)
    bumped_record__59 = _goml_inherent_closure_env_bump_1_closure_env_bump_1_apply(bump__55, 5)
    flipped_record__60 = _goml_inherent_closure_env_flip_2_closure_env_flip_2_apply(flip__56)
    maybe_first__61 = gather(first_record__58)
    maybe_second__62 = gather(bumped_record__59)
    chosen__63 = _goml_choose__T_Maybe_x5b_int32_x5d_(true, maybe_second__62, maybe_first__61)
    stringified__64 = map_maybe__T_int32__U_string(chosen__63, format_total)
    transforms__65 = [2]func(int32) int32{increment, triple}
    results__66 = run_transforms(4, transforms__65)
    first_result__67 = array_get__Array_2_int32(results__66, 0)
    second_result__68 = array_get__Array_2_int32(results__66, 1)
    t69 = first_result__67 < second_result__68
    order_check__69 = t69 && true
    first_text__70 = _goml_trait_impl_Describe_Record_x5b_int32_x5d__describe(first_record__58)
    bumped_text__71 = _goml_trait_impl_Describe_Record_x5b_int32_x5d__describe(bumped_record__59)
    flipped_text__72 = _goml_trait_impl_Describe_Record_x5b_string_x5d__describe(flipped_record__60)
    switch stringified__64.(type) {
    case Maybe__string_Some:
        goto b2
    case Maybe__string_None:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    summary__74 = jp71
    t72 = int32_to_string(first_result__67)
    t73 = int32_to_string(second_result__68)
    t74 = Tuple2_string_string{
        _0: t72,
        _1: t73,
    }
    pair_text__75 = pair_join(t74)
    bool_text__76 = bool_to_string(order_check__69)
    string_println(tracker_info__57)
    string_println(first_text__70)
    string_println(bumped_text__71)
    string_println(flipped_text__72)
    string_println(summary__74)
    string_println(pair_text__75)
    string_println(bool_text__76)
    return struct{}{}
    b2:
    x24 = stringified__64.(Maybe__string_Some)._0
    text__73 = x24
    t75 = "Snapshot: " + text__73
    jp71 = t75
    goto b1
    b3:
    jp71 = "Snapshot: none"
    goto b1
}

func _goml_choose__T_Maybe_x5b_int32_x5d_(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var jp77 Maybe__int32
    if flag__20 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp77
    b2:
    jp77 = when_true__21
    goto b1
    b3:
    jp77 = when_false__22
    goto b1
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var jp79 Maybe__string
    var x10 int32
    var inner__25 int32
    var t80 string
    var t81 Maybe__string
    switch value__23.(type) {
    case Maybe__int32_Some:
        goto b2
    case Maybe__int32_None:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp79
    b2:
    x10 = value__23.(Maybe__int32_Some)._0
    inner__25 = x10
    t80 = f__24(inner__25)
    t81 = Maybe__string_Some{
        _0: t80,
    }
    jp79 = t81
    goto b1
    b3:
    jp79 = Maybe__string_None{}
    goto b1
}

func _goml_inherent_closure_env_snapshot_0_closure_env_snapshot_0_apply(env32 closure_env_snapshot_0) Record__int32 {
    var count__43 *ref_int32_x
    var t82 int32
    var t83 Record__int32
    count__43 = env32.count_0
    t82 = ref_get__Ref_int32(count__43)
    t83 = Record__int32_Value{
        _0: t82,
    }
    return t83
}

func _goml_inherent_closure_env_bump_1_closure_env_bump_1_apply(env33 closure_env_bump_1, delta__47 int32) Record__int32 {
    var count__43 *ref_int32_x
    var before__48 int32
    var t84 int32
    var t85 int32
    var t86 Record__int32
    count__43 = env33.count_0
    before__48 = ref_get__Ref_int32(count__43)
    t84 = before__48 + delta__47
    ref_set__Ref_int32(count__43, t84)
    t85 = ref_get__Ref_int32(count__43)
    t86 = Record__int32_Pair{
        _0: before__48,
        _1: t85,
    }
    return t86
}

func _goml_inherent_closure_env_flip_2_closure_env_flip_2_apply(env34 closure_env_flip_2) Record__string {
    var toggled__44 *ref_bool_x
    var before__50 bool
    var t87 bool
    var after__51 bool
    var t88 string
    var t89 string
    var t90 Record__string
    toggled__44 = env34.toggled_0
    before__50 = ref_get__Ref_bool(toggled__44)
    t87 = !before__50
    ref_set__Ref_bool(toggled__44, t87)
    after__51 = ref_get__Ref_bool(toggled__44)
    t88 = bool_to_string(before__50)
    t89 = bool_to_string(after__51)
    t90 = Record__string_Pair{
        _0: t88,
        _1: t89,
    }
    return t90
}

func main() {
    main0()
}
