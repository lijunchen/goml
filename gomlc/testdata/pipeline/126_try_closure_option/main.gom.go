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

type closure_env_run_0 struct {
    flag_0 bool
    base_1 int32
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t818 Option__i32
    var inline904 int32 = 3
    var inline905 bool = true
    var inline906 closure_env_run_0 = closure_env_run_0{
        flag_0: inline905,
        base_1: inline904,
    }
    var inline907 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline906)
    }
    var inline908 Option__i32 = inline907()
    t818 = inline908
    var t819 string
    switch t818._tag {
    case 0:
        t819 = "none"
    case 1:
        var inline899 int32 = t818._v1_0
        var inline901 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline899)
        var inline902 string = "some=" + inline901
        t819 = inline902
    default:
        panic("non-exhaustive match")
    }
    var inline896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t819)
    _goml_runtime_core_string_println(inline896)
    var t820 Option__i32
    var inline890 int32 = 3
    var inline891 bool = false
    var inline892 closure_env_run_0 = closure_env_run_0{
        flag_0: inline891,
        base_1: inline890,
    }
    var inline893 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline892)
    }
    var inline894 Option__i32 = inline893()
    t820 = inline894
    var t821 string
    switch t820._tag {
    case 0:
        t821 = "none"
    case 1:
        var inline885 int32 = t820._v1_0
        var inline887 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline885)
        var inline888 string = "some=" + inline887
        t821 = inline888
    default:
        panic("non-exhaustive match")
    }
    var inline882 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline882)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline910 int64 = int64(int32(self__286))
    var inline911 string = signed_decimal_string(inline910)
    return inline911
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t838 bool = value__214 < 0
    if t838 {
        var t839 uint64 = uint64(int64(value__214))
        var t840 uint64 = 0 - t839
        var t841 string = decimal_string(t840)
        var t842 string = "-" + t841
        return t842
    } else {
        var t843 uint64 = uint64(int64(value__214))
        var t844 string = decimal_string(t843)
        return t844
    }
}

func decimal_string(value__208 uint64) string {
    var t867 bool = value__208 == 0
    if t867 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop860:
        for {
            var t861 bool = remaining__210 > 0
            if t861 {
                var t862_rhs uint64 = 10
                var t862 uint64 = remaining__210 % t862_rhs
                var t863 uint8 = uint8(uint64(t862))
                var t864 uint8 = t863 + 48
                vec_push__Vec_5uint8(reversed__209, t864)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t865 uint64 = compound_old353 / compound_value354
                remaining__210 = t865
                continue
            } else {
                break Loop_loop860
            }
        }
        var t849 int
        var inline930 int = vec_len__Vec_5uint8(reversed__209)
        t849 = inline930
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t849)
        var offset__212 int = 0
        Loop_loop851:
        for {
            var t852 int
            var inline928 int = vec_len__Vec_5uint8(reversed__209)
            t852 = inline928
            var t853 bool = offset__212 < t852
            if t853 {
                var t854 int
                var inline926 int = vec_len__Vec_5uint8(reversed__209)
                t854 = inline926
                var t855 int = t854 - offset__212
                var t856 int = t855 - 1
                var t857 uint8 = vec_get__Vec_5uint8(reversed__209, t856)
                vec_push__Vec_5uint8(bytes__211, t857)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t858 int = compound_old358 + compound_value359
                offset__212 = t858
                continue
            } else {
                break Loop_loop851
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env801 closure_env_run_0) Option__i32 {
    var flag__2 bool = env801.flag_0
    var base__1 int32 = env801.base_1
    var mtmp796 Option__i32
    if flag__2 {
        var inline932 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 4,
        }
        mtmp796 = inline932
    } else {
        mtmp796 = Option__i32{
            _tag: 0,
        }
    }
    var jp876 int32
    switch mtmp796._tag {
    case 0:
        return Option__i32{
            _tag: 0,
        }
    case 1:
        var x797 int32 = mtmp796._v1_0
        jp876 = x797
        var t877 int32 = jp876 + base__1
        var t878 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t877,
        }
        return t878
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
