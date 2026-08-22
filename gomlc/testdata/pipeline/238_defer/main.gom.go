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

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
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

type closure_env_run_0 struct {}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func early_return() int {
    var defer_return796 int = 7
    var inline970 string = "return:inner"
    var inline971 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline970)
    _goml_runtime_core_string_println(inline971)
    var inline966 string = "return:outer"
    var inline967 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline966)
    _goml_runtime_core_string_println(inline967)
    return defer_return796
}

func maybe(value__0 Option__isize) Option__isize {
    var jp851 int
    switch value__0._tag {
    case 0:
        var defer_return805 Option__isize = Option__isize{
            _tag: 0,
        }
        var inline974 string = "try:cleanup"
        var inline975 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline974)
        _goml_runtime_core_string_println(inline975)
        return defer_return805
    case 1:
        var x804 int = value__0._v1_0
        jp851 = x804
        var defer_result807 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: jp851,
        }
        var inline978 string = "try:cleanup"
        var inline979 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline978)
        _goml_runtime_core_string_println(inline979)
        return defer_result807
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline1003 int = 0
    var inline1004 *ref_int_x = ref__Ref_3int(inline1003)
    index__2 = inline1004
    Loop_loop854:
    for {
        var t855 int
        var inline1001 int = ref_get__Ref_3int(index__2)
        t855 = inline1001
        var t856 bool = t855 < 3
        if t856 {
            var current__3 int
            var inline999 int = ref_get__Ref_3int(index__2)
            current__3 = inline999
            var t857 int = current__3 + 1
            ref_set__Ref_3int(index__2, t857)
            var t861 bool = current__3 == 0
            if t861 {
                var t862 string
                var inline985 string = __goml_builtin_int_to_string(current__3)
                t862 = inline985
                var t863 string = "loop:" + t862
                var inline982 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t863)
                _goml_runtime_core_string_println(inline982)
                continue
            } else {
                var t865 bool = current__3 == 1
                if t865 {
                    var t866 string
                    var inline990 string = __goml_builtin_int_to_string(current__3)
                    t866 = inline990
                    var t867 string = "loop:" + t866
                    var inline987 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t867)
                    _goml_runtime_core_string_println(inline987)
                    break Loop_loop854
                } else {
                    var t859 string
                    var inline995 string = __goml_builtin_int_to_string(current__3)
                    t859 = inline995
                    var t860 string = "loop:" + t859
                    var inline992 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t860)
                    _goml_runtime_core_string_println(inline992)
                    continue
                }
            }
        } else {
            break Loop_loop854
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__isize) int {
    switch value__5._tag {
    case 1:
        var x820 int = value__5._v1_0
        var x823 int = 2
        var defer_tast_result819 int = x820 + x823
        var inline1017 string = "pattern:cleanup"
        var inline1018 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1017)
        _goml_runtime_core_string_println(inline1018)
        return defer_tast_result819
    default:
        var defer_return825 int = 0
        var inline1021 string = "pattern:cleanup"
        var inline1022 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1021)
        _goml_runtime_core_string_println(inline1022)
        return defer_return825
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t878 int = early_return()
    var t879 string
    var inline1079 string = __goml_builtin_int_to_string(t878)
    t879 = inline1079
    var inline1076 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t879)
    _goml_runtime_core_string_println(inline1076)
    maybe(Option__isize{
        _tag: 0,
    })
    loop_cleanup()
    var inline1070 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline1070, "after")
    var inline1072 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline1070)
    var inline1073 string = "observed:" + inline1072
    println__T_string(inline1073)
    var t880 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 3,
    }
    var t881 int = pattern_cleanup(t880)
    var t882 string
    var inline1068 string = __goml_builtin_int_to_string(t881)
    t882 = inline1068
    var inline1065 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t882)
    _goml_runtime_core_string_println(inline1065)
    var t883 int
    var inline1062 int = 0
    println__T_string("pattern:cleanup")
    t883 = inline1062
    var t884 string
    var inline1050 string = __goml_builtin_int_to_string(t883)
    t884 = inline1050
    var inline1047 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t884)
    _goml_runtime_core_string_println(inline1047)
    var inline1041 closure_env_run_0 = closure_env_run_0{}
    var inline1042 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline1041)
    }
    inline1042()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline1037 string = "main:second"
    var inline1038 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1037)
    _goml_runtime_core_string_println(inline1038)
    var inline1033 string = "main:first"
    var inline1034 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1033)
    _goml_runtime_core_string_println(inline1034)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t886 string
    t886 = value__1
    _goml_runtime_core_string_println(t886)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__684 string) *ref_string_x {
    var t901 *ref_string_x = ref__Ref_6string(value__684)
    return t901
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__686 *ref_string_x, value__687 string) struct{} {
    ref_set__Ref_6string(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__685 *ref_string_x) string {
    var t906 string = ref_get__Ref_6string(self__685)
    return t906
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t911 int64 = int64(int(value__222))
    var inline1085 bool = t911 < 0
    if inline1085 {
        var inline1086 uint64 = uint64(int64(t911))
        var inline1087 uint64 = 0 - inline1086
        var inline1088 string = decimal_string(inline1087)
        var inline1089 string = "-" + inline1088
        return inline1089
    } else {
        var inline1090 uint64 = uint64(int64(t911))
        var inline1091 string = decimal_string(inline1090)
        return inline1091
    }
}

func decimal_string(value__208 uint64) string {
    var t946 bool = value__208 == 0
    if t946 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop939:
        for {
            var t940 bool = remaining__210 > 0
            if t940 {
                var t941_rhs uint64 = 10
                var t941 uint64 = remaining__210 % t941_rhs
                var t942 uint8 = uint8(uint64(t941))
                var t943 uint8 = t942 + 48
                vec_push__Vec_5uint8(reversed__209, t943)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t944 uint64 = compound_old353 / compound_value354
                remaining__210 = t944
                continue
            } else {
                break Loop_loop939
            }
        }
        var t928 int
        var inline1101 int = vec_len__Vec_5uint8(reversed__209)
        t928 = inline1101
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t928)
        var offset__212 int = 0
        Loop_loop930:
        for {
            var t931 int
            var inline1099 int = vec_len__Vec_5uint8(reversed__209)
            t931 = inline1099
            var t932 bool = offset__212 < t931
            if t932 {
                var t933 int
                var inline1097 int = vec_len__Vec_5uint8(reversed__209)
                t933 = inline1097
                var t934 int = t933 - offset__212
                var t935 int = t934 - 1
                var t936 uint8 = vec_get__Vec_5uint8(reversed__209, t935)
                vec_push__Vec_5uint8(bytes__211, t936)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t937 int = compound_old358 + compound_value359
                offset__212 = t937
                continue
            } else {
                break Loop_loop930
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env845 closure_env_run_0) struct{} {
    var inline1107 string = "closure:body"
    var inline1108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1107)
    _goml_runtime_core_string_println(inline1108)
    var inline1103 string = "closure:inner"
    var inline1104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1103)
    _goml_runtime_core_string_println(inline1104)
    return struct{}{}
}

func main() {
    main0()
}
