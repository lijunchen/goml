package main

import (
    _goml_os "os"
)

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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type closure_env_goml_builtin_range_inclusive_0 struct {
    finished_0 *ref_bool_x
    current_1 *ref_int_x
    end_2 int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var for_index797 int = 1
    var for_limit798 int = 4
    Loop_loop868:
    for {
        var t869 bool = for_index797 < for_limit798
        if t869 {
            var for_item799 int = for_index797
            var t870 int = for_index797 + 1
            for_index797 = t870
            var inline986 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item799)
            _goml_runtime_core_string_println(inline986)
            continue
        } else {
            break Loop_loop868
        }
    }
    var calls__5 *ref_int_x
    var inline1038 int = 0
    var inline1039 *ref_int_x = ref__Ref_3int(inline1038)
    calls__5 = inline1039
    var for_index803 int
    var inline1033 int = 4
    var inline1034 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(calls__5)
    var inline1035 int = inline1034 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(calls__5, inline1035)
    for_index803 = inline1033
    var for_limit804 int
    var inline1028 int = 6
    var inline1029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(calls__5)
    var inline1030 int = inline1029 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(calls__5, inline1030)
    for_limit804 = inline1028
    var for_done805 bool = for_index803 > for_limit804
    Loop_loop861:
    for {
        var t862 bool = !for_done805
        if t862 {
            var for_item806 int = for_index803
            var t864 bool = for_index803 == for_limit804
            if t864 {
                for_done805 = true
            } else {
                var t866 int = for_index803 + 1
                for_index803 = t866
            }
            var inline989 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item806)
            _goml_runtime_core_string_println(inline989)
            continue
        } else {
            break Loop_loop861
        }
    }
    var for_index810 int = 3
    var for_limit811 int = 1
    var for_done812 bool = for_index810 > for_limit811
    Loop_loop854:
    for {
        var t855 bool = !for_done812
        if t855 {
            var for_item813 int = for_index810
            var t857 bool = for_index810 == for_limit811
            if t857 {
                for_done812 = true
            } else {
                var t859 int = for_index810 + 1
                for_index810 = t859
            }
            var inline992 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item813)
            _goml_runtime_core_string_println(inline992)
            continue
        } else {
            break Loop_loop854
        }
    }
    var maximum__8 int = 9223372036854775807
    var for_index817 int = maximum__8
    var for_done819 bool = for_index817 > maximum__8
    Loop_loop847:
    for {
        var t848 bool = !for_done819
        if t848 {
            var for_item820 int = for_index817
            var t850 bool = for_index817 == maximum__8
            if t850 {
                for_done819 = true
            } else {
                var t852 int = for_index817 + 1
                for_index817 = t852
            }
            var inline995 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(for_item820)
            _goml_runtime_core_string_println(inline995)
            continue
        } else {
            break Loop_loop847
        }
    }
    var iterator__10 FnIterator__isize
    var inline1020 int = 8
    var inline1021 int = 8
    var inline1022 *ref_int_x = ref__Ref_3int(inline1020)
    var inline1023 *ref_bool_x = ref__Ref_4bool(false)
    var inline1024 closure_env_goml_builtin_range_inclusive_0 = closure_env_goml_builtin_range_inclusive_0{
        finished_0: inline1023,
        current_1: inline1022,
        end_2: inline1021,
    }
    var inline1025 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(inline1024)
    }
    var inline1026 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1025)
    iterator__10 = inline1026
    var mtmp824 Option__isize
    var inline1017 func() Option__isize = iterator__10.next_fn
    var inline1018 Option__isize = inline1017()
    mtmp824 = inline1018
    switch mtmp824._tag {
    case 0:
        var inline998 string = "missing"
        var inline999 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline998)
        _goml_runtime_core_string_println(inline999)
    case 1:
        var x825 int = mtmp824._v1_0
        var inline1002 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x825)
        _goml_runtime_core_string_println(inline1002)
    default:
        panic("non-exhaustive match")
    }
    var t843 int
    var inline1015 int = ref_get__Ref_3int(calls__5)
    t843 = inline1015
    var inline1012 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t843)
    _goml_runtime_core_string_println(inline1012)
    var t844 int32
    var inline1008 int32 = 10
    var inline1009 int32 = 20
    var inline1010 int32 = inline1008 + inline1009
    t844 = inline1010
    var inline1005 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t844)
    _goml_runtime_core_string_println(inline1005)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t873 int = ref_get__Ref_3int(self__685)
    return t873
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1048 int64 = int64(int(self__404))
    var inline1049 string = signed_decimal_string(inline1048)
    return inline1049
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__507 func() Option__isize) FnIterator__isize {
    var t902 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__507,
    }
    return t902
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1051 int64 = int64(int32(self__407))
    var inline1052 string = signed_decimal_string(inline1051)
    return inline1052
}

func signed_decimal_string(value__214 int64) string {
    var t920 bool = value__214 < 0
    if t920 {
        var t921 uint64 = uint64(int64(value__214))
        var t922 uint64 = 0 - t921
        var t923 string = decimal_string(t922)
        var t924 string = "-" + t923
        return t924
    } else {
        var t925 uint64 = uint64(int64(value__214))
        var t926 string = decimal_string(t925)
        return t926
    }
}

func decimal_string(value__208 uint64) string {
    var t949 bool = value__208 == 0
    if t949 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop942:
        for {
            var t943 bool = remaining__210 > 0
            if t943 {
                var t944_rhs uint64 = 10
                var t944 uint64 = remaining__210 % t944_rhs
                var t945 uint8 = uint8(uint64(t944))
                var t946 uint8 = t945 + 48
                vec_push__Vec_5uint8(reversed__209, t946)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t947 uint64 = compound_old353 / compound_value354
                remaining__210 = t947
                continue
            } else {
                break Loop_loop942
            }
        }
        var t931 int
        var inline1078 int = vec_len__Vec_5uint8(reversed__209)
        t931 = inline1078
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t931)
        var offset__212 int = 0
        Loop_loop933:
        for {
            var t934 int
            var inline1076 int = vec_len__Vec_5uint8(reversed__209)
            t934 = inline1076
            var t935 bool = offset__212 < t934
            if t935 {
                var t936 int
                var inline1074 int = vec_len__Vec_5uint8(reversed__209)
                t936 = inline1074
                var t937 int = t936 - offset__212
                var t938 int = t937 - 1
                var t939 uint8 = vec_get__Vec_5uint8(reversed__209, t938)
                vec_push__Vec_5uint8(bytes__211, t939)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t940 int = compound_old358 + compound_value359
                offset__212 = t940
                continue
            } else {
                break Loop_loop933
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_hb902f75cf29154a7d4df1174edbd9988_sive__0_i_apply(env829 closure_env_goml_builtin_range_inclusive_0) Option__isize {
    var finished__763 *ref_bool_x = env829.finished_0
    var current__762 *ref_int_x = env829.current_1
    var end__761 int = env829.end_2
    var t978 bool = ref_get__Ref_4bool(finished__763)
    var jp973 bool
    if t978 {
        jp973 = true
    } else {
        var t979 int = ref_get__Ref_3int(current__762)
        var t980 bool = t979 > end__761
        jp973 = t980
    }
    if jp973 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var value__764 int = ref_get__Ref_3int(current__762)
        var t976 bool = value__764 == end__761
        if t976 {
            ref_set__Ref_4bool(finished__763, true)
        } else {
            var t977 int = value__764 + 1
            ref_set__Ref_3int(current__762, t977)
        }
        var t975 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__764,
        }
        return t975
    }
}

func main() {
    main0()
}
