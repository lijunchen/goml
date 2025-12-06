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

func impl_Describe_Tracker_describe(self__0 Tracker) string {
    var ret61 string
    var mtmp0 Tracker = self__0
    var x1 string = mtmp0.label
    var x2 *ref_int32_x = mtmp0.count
    var x3 *ref_bool_x = mtmp0.toggled
    var toggled__3 *ref_bool_x = x3
    var count__2 *ref_int32_x = x2
    var label__1 string = x1
    var current__4 int32 = ref_get__Ref_int32(count__2)
    var flag__5 bool = ref_get__Ref_bool(toggled__3)
    var with_label__6 string = "Tracker(" + label__1
    var with_count_label__7 string = with_label__6 + ", count: "
    var t35 string = int32_to_string(current__4)
    var with_count__8 string = with_count_label__7 + t35
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t37 string = bool_to_string(flag__5)
    var t36 string = with_flag_label__9 + t37
    ret61 = t36 + ")"
    return ret61
}

func impl_Describe_Record_int32_describe(self__10 Record__int32) string {
    var ret62 string
    switch self__10 := self__10.(type) {
    case Record__int32_Value:
        var x4 int32 = self__10._0
        var value__11 int32 = x4
        var t39 string = int32_to_string(value__11)
        var t38 string = "Value(" + t39
        ret62 = t38 + ")"
    case Record__int32_Pair:
        var x5 int32 = self__10._0
        var x6 int32 = self__10._1
        var after__13 int32 = x6
        var before__12 int32 = x5
        var t40 string = int32_to_string(before__12)
        var prefix__14 string = "Pair(" + t40
        var t42 string = prefix__14 + ", "
        var t43 string = int32_to_string(after__13)
        var t41 string = t42 + t43
        ret62 = t41 + ")"
    case Record__int32_Empty:
        ret62 = "Empty"
    }
    return ret62
}

func impl_Describe_Record_string_describe(self__15 Record__string) string {
    var ret63 string
    switch self__15 := self__15.(type) {
    case Record__string_Value:
        var x7 string = self__15._0
        var text__16 string = x7
        var t44 string = "Value(" + text__16
        ret63 = t44 + ")"
    case Record__string_Pair:
        var x8 string = self__15._0
        var x9 string = self__15._1
        var after__18 string = x9
        var before__17 string = x8
        var prefix__19 string = "Pair(" + before__17
        var t46 string = prefix__19 + ", "
        var t45 string = t46 + after__18
        ret63 = t45 + ")"
    case Record__string_Empty:
        ret63 = "Empty"
    }
    return ret63
}

func format_total(total__26 int32) string {
    var ret64 string
    var t47 string = int32_to_string(total__26)
    ret64 = "total: " + t47
    return ret64
}

func increment(value__27 int32) int32 {
    var ret65 int32
    ret65 = value__27 + 1
    return ret65
}

func triple(value__28 int32) int32 {
    var ret66 int32
    ret66 = value__28 * 3
    return ret66
}

func pair_join(parts__29 Tuple2_string_string) string {
    var ret67 string
    var mtmp11 Tuple2_string_string = parts__29
    var x12 string = mtmp11._0
    var x13 string = mtmp11._1
    var right__31 string = x13
    var left__30 string = x12
    var t48 string = left__30 + " -> "
    ret67 = t48 + right__31
    return ret67
}

func run_transforms(value__32 int32, transforms__33 [2]func(int32) int32) [2]int32 {
    var ret68 [2]int32
    var first__34 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 0)
    var second__35 func(int32) int32 = array_get__Array_2_Fn_int32_to_int32(transforms__33, 1)
    var first_result__36 int32 = first__34(value__32)
    var second_result__37 int32 = second__35(first_result__36)
    var t49 [2]int32 = [2]int32{first_result__36, value__32}
    ret68 = array_set__Array_2_int32(t49, 1, second_result__37)
    return ret68
}

func gather(record__38 Record__int32) Maybe__int32 {
    var ret69 Maybe__int32
    switch record__38 := record__38.(type) {
    case Record__int32_Value:
        var x14 int32 = record__38._0
        var value__39 int32 = x14
        ret69 = Maybe__int32_Some{
            _0: value__39,
        }
    case Record__int32_Pair:
        var x16 int32 = record__38._1
        var after__40 int32 = x16
        ret69 = Maybe__int32_Some{
            _0: after__40,
        }
    case Record__int32_Empty:
        ret69 = Maybe__int32_None{}
    }
    return ret69
}

func build_counter(label__41 string, start__42 int32) Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2 {
    var ret70 Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2
    var count__43 *ref_int32_x = ref__Ref_int32(start__42)
    var toggled__44 *ref_bool_x = ref__Ref_bool(false)
    var tracker__45 Tracker = Tracker{
        label: label__41,
        count: count__43,
        toggled: toggled__44,
    }
    var snapshot__46 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__43,
    }
    var bump__49 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__43,
    }
    var flip__52 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__44,
    }
    ret70 = Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2{
        _0: tracker__45,
        _1: snapshot__46,
        _2: bump__49,
        _3: flip__52,
    }
    return ret70
}

func main0() struct{} {
    var ret71 struct{}
    var mtmp19 Tuple4_Tracker_closure_env_snapshot_0_closure_env_bump_1_closure_env_flip_2 = build_counter("goml", 2)
    var x20 Tracker = mtmp19._0
    var x21 closure_env_snapshot_0 = mtmp19._1
    var x22 closure_env_bump_1 = mtmp19._2
    var x23 closure_env_flip_2 = mtmp19._3
    var flip__56 closure_env_flip_2 = x23
    var bump__55 closure_env_bump_1 = x22
    var snapshot__54 closure_env_snapshot_0 = x21
    var tracker__53 Tracker = x20
    var tracker_info__57 string = impl_Describe_Tracker_describe(tracker__53)
    var first_record__58 Record__int32 = impl_inherent_closure_env_snapshot_0_apply(snapshot__54)
    var bumped_record__59 Record__int32 = impl_inherent_closure_env_bump_1_apply(bump__55, 5)
    var flipped_record__60 Record__string = impl_inherent_closure_env_flip_2_apply(flip__56)
    var maybe_first__61 Maybe__int32 = gather(first_record__58)
    var maybe_second__62 Maybe__int32 = gather(bumped_record__59)
    var chosen__63 Maybe__int32 = choose__T_Maybe_int32(true, maybe_second__62, maybe_first__61)
    var stringified__64 Maybe__string = map_maybe__T_int32__U_string(chosen__63, format_total)
    var transforms__65 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__66 [2]int32 = run_transforms(4, transforms__65)
    var first_result__67 int32 = array_get__Array_2_int32(results__66, 0)
    var second_result__68 int32 = array_get__Array_2_int32(results__66, 1)
    var t50 bool = first_result__67 < second_result__68
    var order_check__69 bool = t50 && true
    var first_text__70 string = impl_Describe_Record_int32_describe(first_record__58)
    var bumped_text__71 string = impl_Describe_Record_int32_describe(bumped_record__59)
    var flipped_text__72 string = impl_Describe_Record_string_describe(flipped_record__60)
    var summary__74 string
    switch stringified__64 := stringified__64.(type) {
    case Maybe__string_Some:
        var x24 string = stringified__64._0
        var text__73 string = x24
        summary__74 = "Snapshot: " + text__73
    case Maybe__string_None:
        summary__74 = "Snapshot: none"
    }
    var t52 string = int32_to_string(first_result__67)
    var t53 string = int32_to_string(second_result__68)
    var t51 Tuple2_string_string = Tuple2_string_string{
        _0: t52,
        _1: t53,
    }
    var pair_text__75 string = pair_join(t51)
    var bool_text__76 string = bool_to_string(order_check__69)
    string_println(tracker_info__57)
    string_println(first_text__70)
    string_println(bumped_text__71)
    string_println(flipped_text__72)
    string_println(summary__74)
    string_println(pair_text__75)
    string_println(bool_text__76)
    ret71 = struct{}{}
    return ret71
}

func choose__T_Maybe_int32(flag__20 bool, when_true__21 Maybe__int32, when_false__22 Maybe__int32) Maybe__int32 {
    var ret72 Maybe__int32
    if flag__20 {
        ret72 = when_true__21
    } else {
        ret72 = when_false__22
    }
    return ret72
}

func map_maybe__T_int32__U_string(value__23 Maybe__int32, f__24 func(int32) string) Maybe__string {
    var ret73 Maybe__string
    switch value__23 := value__23.(type) {
    case Maybe__int32_Some:
        var x10 int32 = value__23._0
        var inner__25 int32 = x10
        var t54 string = f__24(inner__25)
        ret73 = Maybe__string_Some{
            _0: t54,
        }
    case Maybe__int32_None:
        ret73 = Maybe__string_None{}
    }
    return ret73
}

func impl_inherent_closure_env_snapshot_0_apply(env32 closure_env_snapshot_0) Record__int32 {
    var ret74 Record__int32
    var count__43 *ref_int32_x = env32.count_0
    var t55 int32 = ref_get__Ref_int32(count__43)
    ret74 = Record__int32_Value{
        _0: t55,
    }
    return ret74
}

func impl_inherent_closure_env_bump_1_apply(env33 closure_env_bump_1, delta__47 int32) Record__int32 {
    var ret75 Record__int32
    var count__43 *ref_int32_x = env33.count_0
    var before__48 int32 = ref_get__Ref_int32(count__43)
    var t56 int32 = before__48 + delta__47
    ref_set__Ref_int32(count__43, t56)
    var t57 int32 = ref_get__Ref_int32(count__43)
    ret75 = Record__int32_Pair{
        _0: before__48,
        _1: t57,
    }
    return ret75
}

func impl_inherent_closure_env_flip_2_apply(env34 closure_env_flip_2) Record__string {
    var ret76 Record__string
    var toggled__44 *ref_bool_x = env34.toggled_0
    var before__50 bool = ref_get__Ref_bool(toggled__44)
    var t58 bool = !before__50
    ref_set__Ref_bool(toggled__44, t58)
    var after__51 bool = ref_get__Ref_bool(toggled__44)
    var t59 string = bool_to_string(before__50)
    var t60 string = bool_to_string(after__51)
    ret76 = Record__string_Pair{
        _0: t59,
        _1: t60,
    }
    return ret76
}

func main() {
    main0()
}
