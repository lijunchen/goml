package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
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

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_main_4 struct {}

type closure_env_main_5 struct {}

type closure_env_main_6 struct {}

type closure_env_main_7 struct {}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__isize__isize struct {
    _tag int32
    _v0_0 int
    _v1_0 int
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func main0() struct{} {
    var some__0 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 3,
    }
    var t812 closure_env_main_0 = closure_env_main_0{}
    var t813 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t812, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(some__0, t813)
    var t814 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t814)
    var t815 closure_env_main_1 = closure_env_main_1{}
    var t816 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t815, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(some__0, t816)
    var t817 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t817)
    var t818 closure_env_main_2 = closure_env_main_2{}
    var t819 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t818, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__isize____U__string(some__0, t819)
    var t820 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t820)
    var none__7 Option__isize = Option__isize{
        _tag: 0,
    }
    var converted__8 Result__isize__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__isize(none__7, "none")
    var t821 closure_env_main_3 = closure_env_main_3{}
    var t822 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t821, p0)
    }
    var t823 int = _goml_m_inherent_i_Result_i_Re_had11e393bde0ae88c9d8324ffd70f925_ing____T__isize(converted__8, t822)
    println__T_isize(t823)
    var ok__10 Result__isize__string = Result__isize__string{
        _tag: 0,
        _v0_0: 5,
    }
    var t824 closure_env_main_4 = closure_env_main_4{}
    var t825 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t824, p0)
    }
    var t826 Result__isize__string = _goml_m_inherent_i_Result_i_Re_hf15fd215f39b8121388b37682eabc3c0_ize____U__isize(ok__10, t825)
    var t827 int
    var inline1018 int = 0
    switch t826._tag {
    case 0:
        var inline1019 int = t826._v0_0
        t827 = inline1019
    case 1:
        t827 = inline1018
    default:
        panic("non-exhaustive match")
    }
    var inline1015 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t827)
    _goml_runtime_core_string_println(inline1015)
    var t828 closure_env_main_5 = closure_env_main_5{}
    var t829 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t828, p0)
    }
    var mapped_error__14 Result__isize__isize
    var inline1010 string = "bad"
    var inline1012 int = t829(inline1010)
    var inline1013 Result__isize__isize = Result__isize__isize{
        _tag: 1,
        _v1_0: inline1012,
    }
    mapped_error__14 = inline1013
    var t830 closure_env_main_6 = closure_env_main_6{}
    var t831 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t830, p0)
    }
    var t832 int
    switch mapped_error__14._tag {
    case 0:
        var inline1001 int = mapped_error__14._v0_0
        t832 = inline1001
    case 1:
        var inline1003 int = mapped_error__14._v1_0
        var inline1005 int = t831(inline1003)
        t832 = inline1005
    default:
        panic("non-exhaustive match")
    }
    var inline998 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t832)
    _goml_runtime_core_string_println(inline998)
    var t833 closure_env_main_7 = closure_env_main_7{}
    var t834 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t833, p0)
    }
    var next__17 Result__string__string
    var inline991 int = 5
    var inline993 Result__string__string = t834(inline991)
    next__17 = inline993
    var t835 string
    var inline987 string = "missing"
    switch next__17._tag {
    case 0:
        var inline988 string = next__17._v0_0
        t835 = inline988
    case 1:
        t835 = inline987
    default:
        panic("non-exhaustive match")
    }
    var inline984 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline984)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(self__735 Option__isize, map_fn__736 func(int) string) Option__string {
    switch self__735._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x783 int = self__735._v1_0
        var t843 string = map_fn__736(x783)
        var t844 Option__string = Option__string{
            _tag: 1,
            _v1_0: t843,
        }
        return t844
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t846 string
    t846 = value__1
    _goml_runtime_core_string_println(t846)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__720 Option__string, fallback__721 string) string {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 string = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__isize____U__string(self__738 Option__isize, next__739 func(int) Option__string) Option__string {
    switch self__738._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x784 int = self__738._v1_0
        var t856 Option__string = next__739(x784)
        return t856
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__isize(self__741 Option__isize, error__742 string) Result__isize__string {
    switch self__741._tag {
    case 0:
        var t861 Result__isize__string = Result__isize__string{
            _tag: 1,
            _v1_0: error__742,
        }
        return t861
    case 1:
        var x785 int = self__741._v1_0
        var t862 Result__isize__string = Result__isize__string{
            _tag: 0,
            _v0_0: x785,
        }
        return t862
    default:
        panic("non-exhaustive match")
    }
}

func println__T_isize(value__1 int) struct{} {
    var t864 string
    var inline1026 string = __goml_builtin_int_to_string(value__1)
    t864 = inline1026
    _goml_runtime_core_string_println(t864)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_had11e393bde0ae88c9d8324ffd70f925_ing____T__isize(self__731 Result__isize__string, fallback__732 func(string) int) int {
    switch self__731._tag {
    case 0:
        var x781 int = self__731._v0_0
        return x781
    case 1:
        var x782 string = self__731._v1_0
        var t873 int = fallback__732(x782)
        return t873
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hf15fd215f39b8121388b37682eabc3c0_ize____U__isize(self__744 Result__isize__string, map_fn__745 func(int) int) Result__isize__string {
    switch self__744._tag {
    case 0:
        var x786 int = self__744._v0_0
        var t878 int = map_fn__745(x786)
        var t879 Result__isize__string = Result__isize__string{
            _tag: 0,
            _v0_0: t878,
        }
        return t879
    case 1:
        var x787 string = self__744._v1_0
        var t880 Result__isize__string = Result__isize__string{
            _tag: 1,
            _v1_0: x787,
        }
        return t880
    default:
        panic("non-exhaustive match")
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t909 int64 = int64(int(value__222))
    var inline1028 bool = t909 < 0
    if inline1028 {
        var inline1029 uint64 = uint64(int64(t909))
        var inline1030 uint64 = 0 - inline1029
        var inline1031 string = decimal_string(inline1030)
        var inline1032 string = "-" + inline1031
        return inline1032
    } else {
        var inline1033 uint64 = uint64(int64(t909))
        var inline1034 string = decimal_string(inline1033)
        return inline1034
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1036 int64 = int64(int(self__404))
    var inline1037 string = signed_decimal_string(inline1036)
    return inline1037
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
        var inline1047 int = vec_len__Vec_5uint8(reversed__209)
        t931 = inline1047
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t931)
        var offset__212 int = 0
        Loop_loop933:
        for {
            var t934 int
            var inline1045 int = vec_len__Vec_5uint8(reversed__209)
            t934 = inline1045
            var t935 bool = offset__212 < t934
            if t935 {
                var t936 int
                var inline1043 int = vec_len__Vec_5uint8(reversed__209)
                t936 = inline1043
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

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env803 closure_env_main_0, value__1 int) string {
    var inline1049 string = __goml_builtin_int_to_string(value__1)
    return inline1049
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env804 closure_env_main_1, value__3 int) string {
    var t960 string
    var inline1051 string = __goml_builtin_int_to_string(value__3)
    t960 = inline1051
    var t961 string = "static:" + t960
    return t961
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env805 closure_env_main_2, value__5 int) Option__string {
    var t964 string
    var inline1053 string = __goml_builtin_int_to_string(value__5)
    t964 = inline1053
    var t965 string = "value:" + t964
    var t966 Option__string = Option__string{
        _tag: 1,
        _v1_0: t965,
    }
    return t966
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env806 closure_env_main_3, error__9 string) int {
    var inline1055 int = _goml_runtime_core_string_len(error__9)
    return inline1055
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env807 closure_env_main_4, value__11 int) int {
    var t972 int = value__11 + 2
    return t972
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env808 closure_env_main_5, value__13 string) int {
    var inline1057 int = _goml_runtime_core_string_len(value__13)
    return inline1057
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env809 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env810 closure_env_main_7, value__16 int) Result__string__string {
    var t980 string
    var inline1059 string = __goml_builtin_int_to_string(value__16)
    t980 = inline1059
    var t981 string = "next:" + t980
    var t982 Result__string__string = Result__string__string{
        _tag: 0,
        _v0_0: t981,
    }
    return t982
}

func main() {
    main0()
}
