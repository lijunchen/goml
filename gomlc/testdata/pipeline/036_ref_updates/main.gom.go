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

type ref_Ref_5int32_x struct {
    value *ref_int32_x
}

func ref__Ref_10Ref_5int32(value *ref_int32_x) *ref_Ref_5int32_x {
    return &ref_Ref_5int32_x{
        value: value,
    }
}

func ref_get__Ref_10Ref_5int32(reference *ref_Ref_5int32_x) *ref_int32_x {
    return reference.value
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

type Ordering int32

func bump(cell__0 *ref_int32_x) int32 {
    var t809 int32
    var inline955 int32 = ref_get__Ref_5int32(cell__0)
    t809 = inline955
    var t810 int32 = t809 + 1
    ref_set__Ref_5int32(cell__0, t810)
    var inline951 int32 = ref_get__Ref_5int32(cell__0)
    return inline951
}

func flip(flag__1 *ref_bool_x) bool {
    var current__2 bool
    var inline961 bool = ref_get__Ref_4bool(flag__1)
    current__2 = inline961
    var t814 bool = !current__2
    ref_set__Ref_4bool(flag__1, t814)
    var inline957 bool = ref_get__Ref_4bool(flag__1)
    return inline957
}

func nested_total(cell__3 *ref_Ref_5int32_x) int32 {
    var inner__4 *ref_int32_x
    var inline969 *ref_int32_x = ref_get__Ref_10Ref_5int32(cell__3)
    inner__4 = inline969
    var before__5 int32
    var inline967 int32 = ref_get__Ref_5int32(inner__4)
    before__5 = inline967
    var t818 int32 = before__5 + 2
    ref_set__Ref_5int32(inner__4, t818)
    var t819 int32
    var inline963 int32 = ref_get__Ref_5int32(inner__4)
    t819 = inline963
    var t820 int32 = before__5 + t819
    return t820
}

func main0() struct{} {
    var counter__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(39)
    var toggler__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var t840 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(3)
    var nested__14 *ref_Ref_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_i32_r_(t840)
    var bumped__15 int32 = bump(counter__12)
    var flipped__16 bool = flip(toggler__13)
    var flipped_again__17 bool = flip(toggler__13)
    var inner__18 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(nested__14)
    var t841 int32
    var inline1047 int32 = ref_get__Ref_5int32(inner__18)
    t841 = inline1047
    var t842 int32 = t841 + bumped__15
    ref_set__Ref_5int32(inner__18, t842)
    var nested_total_val__19 int32 = nested_total(nested__14)
    var alias_total__20 int32
    var inline1040 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__12)
    var inline1041 int32 = inline1040 + 5
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(counter__12, inline1041)
    var inline1043 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__12)
    alias_total__20 = inline1043
    var pair_total__21 int32
    var inline1029 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(4)
    var inline1030 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(6)
    var inline1031 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1029)
    var inline1032 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1030)
    var inline1033 int32 = inline1031 + inline1032
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1029, inline1033)
    var inline1035 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1029)
    var inline1036 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1030)
    var inline1037 int32 = inline1035 + inline1036
    pair_total__21 = inline1037
    var reassigned__22 int32
    var inline1023 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(nested__14)
    var inline1024 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1023)
    var inline1025 int32 = inline1024 + 7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1023, inline1025)
    var inline1027 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1023)
    reassigned__22 = inline1027
    var bool_check__23 bool = !false
    var t843 int32
    var inline1021 int32 = ref_get__Ref_5int32(counter__12)
    t843 = inline1021
    var t844 int32 = bumped__15 + t843
    var t845 string
    var inline1019 string = __goml_builtin_int32_to_string(t844)
    t845 = inline1019
    var inline1016 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t845)
    _goml_runtime_core_string_println(inline1016)
    var t846 int32 = nested_total_val__19 + alias_total__20
    var t847 int32 = t846 + reassigned__22
    var t848 string
    var inline1014 string = __goml_builtin_int32_to_string(t847)
    t848 = inline1014
    var inline1011 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t848)
    _goml_runtime_core_string_println(inline1011)
    var t849 string
    var inline1009 string = __goml_builtin_int32_to_string(pair_total__21)
    t849 = inline1009
    var inline1006 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t849)
    _goml_runtime_core_string_println(inline1006)
    var jp854 bool
    if flipped__16 {
        jp854 = flipped_again__17
    } else {
        jp854 = false
    }
    var jp851 bool
    if jp854 {
        jp851 = bool_check__23
    } else {
        jp851 = false
    }
    var t852 string
    var inline1004 string = _goml_runtime_core_bool_to_string(jp851)
    t852 = inline1004
    var inline1001 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t852)
    _goml_runtime_core_string_println(inline1001)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t857 int32 = ref_get__Ref_5int32(self__685)
    return t857
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Ref_l_i32_r_(self__685 *ref_Ref_5int32_x) *ref_int32_x {
    var t867 *ref_int32_x = ref_get__Ref_10Ref_5int32(self__685)
    return t867
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t870 *ref_int32_x = ref__Ref_5int32(value__684)
    return t870
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__684 bool) *ref_bool_x {
    var t873 *ref_bool_x = ref__Ref_4bool(value__684)
    return t873
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Ref_l_i32_r_(value__684 *ref_int32_x) *ref_Ref_5int32_x {
    var t876 *ref_Ref_5int32_x = ref__Ref_10Ref_5int32(value__684)
    return t876
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t890 int64 = int64(int32(value__225))
    var inline1053 bool = t890 < 0
    if inline1053 {
        var inline1054 uint64 = uint64(int64(t890))
        var inline1055 uint64 = 0 - inline1054
        var inline1056 string = decimal_string(inline1055)
        var inline1057 string = "-" + inline1056
        return inline1057
    } else {
        var inline1058 uint64 = uint64(int64(t890))
        var inline1059 string = decimal_string(inline1058)
        return inline1059
    }
}

func decimal_string(value__208 uint64) string {
    var t925 bool = value__208 == 0
    if t925 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop918:
        for {
            var t919 bool = remaining__210 > 0
            if t919 {
                var t920_rhs uint64 = 10
                var t920 uint64 = remaining__210 % t920_rhs
                var t921 uint8 = uint8(uint64(t920))
                var t922 uint8 = t921 + 48
                vec_push__Vec_5uint8(reversed__209, t922)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t923 uint64 = compound_old353 / compound_value354
                remaining__210 = t923
                continue
            } else {
                break Loop_loop918
            }
        }
        var t907 int
        var inline1069 int = vec_len__Vec_5uint8(reversed__209)
        t907 = inline1069
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t907)
        var offset__212 int = 0
        Loop_loop909:
        for {
            var t910 int
            var inline1067 int = vec_len__Vec_5uint8(reversed__209)
            t910 = inline1067
            var t911 bool = offset__212 < t910
            if t911 {
                var t912 int
                var inline1065 int = vec_len__Vec_5uint8(reversed__209)
                t912 = inline1065
                var t913 int = t912 - offset__212
                var t914 int = t913 - 1
                var t915 uint8 = vec_get__Vec_5uint8(reversed__209, t914)
                vec_push__Vec_5uint8(bytes__211, t915)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t916 int = compound_old358 + compound_value359
                offset__212 = t916
                continue
            } else {
                break Loop_loop909
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
