package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string struct {
    _0 Tracker
    _1 func() Record__i32
    _2 func(int32) Record__i32
    _3 func() Record__string
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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

type Ordering int32

type Record__i32 struct {
    _tag int32
    _v0_0 int32
    _v1_0 int32
    _v1_1 int32
}

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

type Maybe__i32 struct {
    _tag int32
    _v0_0 int32
}

type Maybe__string struct {
    _tag int32
    _v0_0 string
}

func _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(self__0 Tracker) string {
    var x797 string = self__0.label
    var x798 *ref_int32_x = self__0.count
    var x799 *ref_bool_x = self__0.toggled
    var current__4 int32
    var inline1018 int32 = ref_get__Ref_5int32(x798)
    current__4 = inline1018
    var flag__5 bool
    var inline1016 bool = ref_get__Ref_4bool(x799)
    flag__5 = inline1016
    var with_label__6 string = "Tracker(" + x797
    var with_count_label__7 string = with_label__6 + ", count: "
    var t838 string
    var inline1014 string = __goml_builtin_int32_to_string(current__4)
    t838 = inline1014
    var with_count__8 string = with_count_label__7 + t838
    var with_flag_label__9 string = with_count__8 + ", toggled: "
    var t839 string
    var inline1012 string = _goml_runtime_core_bool_to_string(flag__5)
    t839 = inline1012
    var t840 string = with_flag_label__9 + t839
    var t841 string = t840 + ")"
    return t841
}

func _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(self__10 Record__i32) string {
    switch self__10._tag {
    case 0:
        var x800 int32 = self__10._v0_0
        var t846 string
        var inline1020 string = __goml_builtin_int32_to_string(x800)
        t846 = inline1020
        var t847 string = "Value(" + t846
        var t848 string = t847 + ")"
        return t848
    case 1:
        var x801 int32 = self__10._v1_0
        var x802 int32 = self__10._v1_1
        var t849 string
        var inline1024 string = __goml_builtin_int32_to_string(x801)
        t849 = inline1024
        var prefix__14 string = "Pair(" + t849
        var t850 string = prefix__14 + ", "
        var t851 string
        var inline1022 string = __goml_builtin_int32_to_string(x802)
        t851 = inline1022
        var t852 string = t850 + t851
        var t853 string = t852 + ")"
        return t853
    case 2:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(self__15 Record__string) string {
    switch self__15.(type) {
    case Record__string_Value:
        var x803 string = self__15.(Record__string_Value)._0
        var t858 string = "Value(" + x803
        var t859 string = t858 + ")"
        return t859
    case Record__string_Pair:
        var x804 string = self__15.(Record__string_Pair)._0
        var x805 string = self__15.(Record__string_Pair)._1
        var prefix__19 string = "Pair(" + x804
        var t860 string = prefix__19 + ", "
        var t861 string = t860 + x805
        var t862 string = t861 + ")"
        return t862
    case Record__string_Empty:
        return "Empty"
    default:
        panic("non-exhaustive match")
    }
}

func format_total(total__26 int32) string {
    var t865 string
    var inline1026 string = __goml_builtin_int32_to_string(total__26)
    t865 = inline1026
    var t866 string = "total: " + t865
    return t866
}

func increment(value__27 int32) int32 {
    var t869 int32 = value__27 + 1
    return t869
}

func triple(value__28 int32) int32 {
    var t872 int32 = value__28 * 3
    return t872
}

func gather(record__39 Record__i32) Maybe__i32 {
    switch record__39._tag {
    case 0:
        var x815 int32 = record__39._v0_0
        var t885 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: x815,
        }
        return t885
    case 1:
        var x817 int32 = record__39._v1_1
        var t886 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: x817,
        }
        return t886
    case 2:
        return Maybe__i32{
            _tag: 1,
        }
    default:
        panic("non-exhaustive match")
    }
}

func build_counter(label__42 string, start__43 int32) Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string {
    var count__44 *ref_int32_x
    var inline1031 *ref_int32_x = ref__Ref_5int32(start__43)
    count__44 = inline1031
    var toggled__45 *ref_bool_x
    var inline1028 bool = false
    var inline1029 *ref_bool_x = ref__Ref_4bool(inline1028)
    toggled__45 = inline1029
    var tracker__46 Tracker = Tracker{
        label: label__42,
        count: count__44,
        toggled: toggled__45,
    }
    var t889 closure_env_snapshot_0 = closure_env_snapshot_0{
        count_0: count__44,
    }
    var snapshot__47 func() Record__i32 = func() Record__i32 {
        return _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(t889)
    }
    var t890 closure_env_bump_1 = closure_env_bump_1{
        count_0: count__44,
    }
    var bump__50 func(int32) Record__i32 = func(p0 int32) Record__i32 {
        return _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(t890, p0)
    }
    var t891 closure_env_flip_2 = closure_env_flip_2{
        toggled_0: toggled__45,
    }
    var flip__53 func() Record__string = func() Record__string {
        return _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(t891)
    }
    var t892 Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string = Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string{
        _0: tracker__46,
        _1: snapshot__47,
        _2: bump__50,
        _3: flip__53,
    }
    return t892
}

func main0() struct{} {
    var mtmp820 Tuple4_7Tracker_24TFunc0_ret_1_hd2fccc77024d42a64d1c9425814244a6_4Record__string = build_counter("goml", 2)
    var x821 Tracker = mtmp820._0
    var x822 func() Record__i32 = mtmp820._1
    var x823 func(int32) Record__i32 = mtmp820._2
    var x824 func() Record__string = mtmp820._3
    var tracker_info__58 string = _goml_m_trait__impl_i_Describe_i_Tracker_i_describe(x821)
    var first_record__59 Record__i32 = x822()
    var bumped_record__60 Record__i32 = x823(5)
    var flipped_record__61 Record__string = x824()
    var maybe_first__62 Maybe__i32 = gather(first_record__59)
    var maybe_second__63 Maybe__i32 = gather(bumped_record__60)
    var chosen__64 Maybe__i32 = _goml_m_choose____T__Maybe_l_i32_r_(true, maybe_second__63, maybe_first__62)
    var stringified__65 Maybe__string = map_maybe__T_i32__U_string(chosen__64, format_total)
    var transforms__66 [2]func(int32) int32 = [2]func(int32) int32{increment, triple}
    var results__67 [2]int32
    var inline1068 int32 = 4
    var inline1069 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 0)
    var inline1070 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(transforms__66, 1)
    var inline1071 int32 = inline1069(inline1068)
    var inline1072 int32 = inline1070(inline1071)
    var inline1073 [2]int32 = [2]int32{inline1071, inline1068}
    var inline1074 [2]int32 = inline1073
    var inline1075 int = 1
    array_get__Array_2_5int32(inline1074, inline1075)
    var inline1078 [2]int32 = array_set__Array_2_5int32(inline1074, inline1075, inline1072)
    inline1073 = inline1078
    results__67 = inline1073
    var first_result__68 int32 = array_get__Array_2_5int32(results__67, 0)
    var second_result__69 int32 = array_get__Array_2_5int32(results__67, 1)
    var t902 bool = first_result__68 < second_result__69
    var jp895 bool
    if t902 {
        jp895 = true
    } else {
        jp895 = false
    }
    var first_text__71 string = _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(first_record__59)
    var bumped_text__72 string = _goml_m_trait__impl_i_Describe_i_Record____i32_i_describe(bumped_record__60)
    var flipped_text__73 string = _goml_m_trait__impl_i_Describe_i_Record____string_i_describe(flipped_record__61)
    var jp897 string
    switch stringified__65._tag {
    case 0:
        var x825 string = stringified__65._v0_0
        var t901 string = "Snapshot: " + x825
        jp897 = t901
    case 1:
        jp897 = "Snapshot: none"
    default:
        panic("non-exhaustive match")
    }
    var t898 string
    var inline1066 string = __goml_builtin_int32_to_string(first_result__68)
    t898 = inline1066
    var t899 string
    var inline1064 string = __goml_builtin_int32_to_string(second_result__69)
    t899 = inline1064
    var pair_text__76 string
    var inline1061 string = t898 + " -> "
    var inline1062 string = inline1061 + t899
    pair_text__76 = inline1062
    var bool_text__77 string
    var inline1054 string = _goml_runtime_core_bool_to_string(jp895)
    bool_text__77 = inline1054
    var inline1051 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(tracker_info__58)
    _goml_runtime_core_string_println(inline1051)
    var inline1048 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first_text__71)
    _goml_runtime_core_string_println(inline1048)
    var inline1045 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bumped_text__72)
    _goml_runtime_core_string_println(inline1045)
    var inline1042 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(flipped_text__73)
    _goml_runtime_core_string_println(inline1042)
    var inline1039 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp897)
    _goml_runtime_core_string_println(inline1039)
    var inline1036 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(pair_text__76)
    _goml_runtime_core_string_println(inline1036)
    var inline1033 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(bool_text__77)
    _goml_runtime_core_string_println(inline1033)
    return struct{}{}
}

func _goml_m_choose____T__Maybe_l_i32_r_(flag__20 bool, when_true__21 Maybe__i32, when_false__22 Maybe__i32) Maybe__i32 {
    if flag__20 {
        return when_true__21
    } else {
        return when_false__22
    }
}

func map_maybe__T_i32__U_string(value__23 Maybe__i32, f__24 func(int32) string) Maybe__string {
    switch value__23._tag {
    case 0:
        var x806 int32 = value__23._v0_0
        var t933 string = f__24(x806)
        var t934 Maybe__string = Maybe__string{
            _tag: 0,
            _v0_0: t933,
        }
        return t934
    case 1:
        return Maybe__string{
            _tag: 1,
        }
    default:
        panic("non-exhaustive match")
    }
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t940 int64 = int64(int32(value__225))
    var inline1086 bool = t940 < 0
    if inline1086 {
        var inline1087 uint64 = uint64(int64(t940))
        var inline1088 uint64 = 0 - inline1087
        var inline1089 string = decimal_string(inline1088)
        var inline1090 string = "-" + inline1089
        return inline1090
    } else {
        var inline1091 uint64 = uint64(int64(t940))
        var inline1092 string = decimal_string(inline1091)
        return inline1092
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t977 bool = value__208 == 0
    if t977 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop970:
        for {
            var t971 bool = remaining__210 > 0
            if t971 {
                var t972_rhs uint64 = 10
                var t972 uint64 = remaining__210 % t972_rhs
                var t973 uint8 = uint8(uint64(t972))
                var t974 uint8 = t973 + 48
                vec_push__Vec_5uint8(reversed__209, t974)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t975 uint64 = compound_old353 / compound_value354
                remaining__210 = t975
                continue
            } else {
                break Loop_loop970
            }
        }
        var t959 int
        var inline1102 int = vec_len__Vec_5uint8(reversed__209)
        t959 = inline1102
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t959)
        var offset__212 int = 0
        Loop_loop961:
        for {
            var t962 int
            var inline1100 int = vec_len__Vec_5uint8(reversed__209)
            t962 = inline1100
            var t963 bool = offset__212 < t962
            if t963 {
                var t964 int
                var inline1098 int = vec_len__Vec_5uint8(reversed__209)
                t964 = inline1098
                var t965 int = t964 - offset__212
                var t966 int = t965 - 1
                var t967 uint8 = vec_get__Vec_5uint8(reversed__209, t966)
                vec_push__Vec_5uint8(bytes__211, t967)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t968 int = compound_old358 + compound_value359
                offset__212 = t968
                continue
            } else {
                break Loop_loop961
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__snapshot__0_i_closure__env__snapshot__0_i_apply(env833 closure_env_snapshot_0) Record__i32 {
    var count__44 *ref_int32_x = env833.count_0
    var t998 int32
    var inline1104 int32 = ref_get__Ref_5int32(count__44)
    t998 = inline1104
    var t999 Record__i32 = Record__i32{
        _tag: 0,
        _v0_0: t998,
    }
    return t999
}

func _goml_m_inherent_i_closure__env__bump__1_i_closure__env__bump__1_i_apply(env834 closure_env_bump_1, delta__48 int32) Record__i32 {
    var count__44 *ref_int32_x = env834.count_0
    var before__49 int32
    var inline1110 int32 = ref_get__Ref_5int32(count__44)
    before__49 = inline1110
    var t1002 int32 = before__49 + delta__48
    ref_set__Ref_5int32(count__44, t1002)
    var t1003 int32
    var inline1106 int32 = ref_get__Ref_5int32(count__44)
    t1003 = inline1106
    var t1004 Record__i32 = Record__i32{
        _tag: 1,
        _v1_0: before__49,
        _v1_1: t1003,
    }
    return t1004
}

func _goml_m_inherent_i_closure__env__flip__2_i_closure__env__flip__2_i_apply(env835 closure_env_flip_2) Record__string {
    var toggled__45 *ref_bool_x = env835.toggled_0
    var before__51 bool
    var inline1120 bool = ref_get__Ref_4bool(toggled__45)
    before__51 = inline1120
    var t1007 bool = !before__51
    ref_set__Ref_4bool(toggled__45, t1007)
    var after__52 bool
    var inline1116 bool = ref_get__Ref_4bool(toggled__45)
    after__52 = inline1116
    var t1008 string
    var inline1114 string = _goml_runtime_core_bool_to_string(before__51)
    t1008 = inline1114
    var t1009 string
    var inline1112 string = _goml_runtime_core_bool_to_string(after__52)
    t1009 = inline1112
    var t1010 Record__string = Record__string_Pair{
        _0: t1008,
        _1: t1009,
    }
    return t1010
}

func main() {
    main0()
}
