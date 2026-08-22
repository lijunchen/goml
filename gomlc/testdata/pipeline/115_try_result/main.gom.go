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
}

type Ordering int32

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func main0() struct{} {
    var t825 Result__i32__string
    var inline917 bool = true
    var inline918 closure_env_run_0 = closure_env_run_0{
        flag_0: inline917,
    }
    var inline919 func() Result__i32__string = func() Result__i32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline918)
    }
    var inline920 Result__i32__string = inline919()
    t825 = inline920
    var t826 string
    switch t825._tag {
    case 0:
        var inline909 int32 = t825._v0_0
        var inline911 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline909)
        var inline912 string = "ok=" + inline911
        t826 = inline912
    case 1:
        var inline913 string = t825._v1_0
        var inline915 string = "err=" + inline913
        t826 = inline915
    default:
        panic("non-exhaustive match")
    }
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline906)
    var t827 Result__i32__string
    var inline901 bool = false
    var inline902 closure_env_run_0 = closure_env_run_0{
        flag_0: inline901,
    }
    var inline903 func() Result__i32__string = func() Result__i32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline902)
    }
    var inline904 Result__i32__string = inline903()
    t827 = inline904
    var t828 string
    switch t827._tag {
    case 0:
        var inline893 int32 = t827._v0_0
        var inline895 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline893)
        var inline896 string = "ok=" + inline895
        t828 = inline896
    case 1:
        var inline897 string = t827._v1_0
        var inline899 string = "err=" + inline897
        t828 = inline899
    default:
        panic("non-exhaustive match")
    }
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t828)
    _goml_runtime_core_string_println(inline890)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline922 int64 = int64(int32(self__286))
    var inline923 string = signed_decimal_string(inline922)
    return inline923
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t845 bool = value__214 < 0
    if t845 {
        var t846 uint64 = uint64(int64(value__214))
        var t847 uint64 = 0 - t846
        var t848 string = decimal_string(t847)
        var t849 string = "-" + t848
        return t849
    } else {
        var t850 uint64 = uint64(int64(value__214))
        var t851 string = decimal_string(t850)
        return t851
    }
}

func decimal_string(value__208 uint64) string {
    var t874 bool = value__208 == 0
    if t874 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop867:
        for {
            var t868 bool = remaining__210 > 0
            if t868 {
                var t869_rhs uint64 = 10
                var t869 uint64 = remaining__210 % t869_rhs
                var t870 uint8 = uint8(uint64(t869))
                var t871 uint8 = t870 + 48
                vec_push__Vec_5uint8(reversed__209, t871)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t872 uint64 = compound_old353 / compound_value354
                remaining__210 = t872
                continue
            } else {
                break Loop_loop867
            }
        }
        var t856 int
        var inline942 int = vec_len__Vec_5uint8(reversed__209)
        t856 = inline942
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t856)
        var offset__212 int = 0
        Loop_loop858:
        for {
            var t859 int
            var inline940 int = vec_len__Vec_5uint8(reversed__209)
            t859 = inline940
            var t860 bool = offset__212 < t859
            if t860 {
                var t861 int
                var inline938 int = vec_len__Vec_5uint8(reversed__209)
                t861 = inline938
                var t862 int = t861 - offset__212
                var t863 int = t862 - 1
                var t864 uint8 = vec_get__Vec_5uint8(reversed__209, t863)
                vec_push__Vec_5uint8(bytes__211, t864)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t865 int = compound_old358 + compound_value359
                offset__212 = t865
                continue
            } else {
                break Loop_loop858
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env803 closure_env_run_0) Result__i32__string {
    var flag__3 bool = env803.flag_0
    var mtmp796 Result__i32__string
    if flag__3 {
        var inline947 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: 7,
        }
        mtmp796 = inline947
    } else {
        var inline948 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: "nope",
        }
        mtmp796 = inline948
    }
    var jp883 int32
    switch mtmp796._tag {
    case 0:
        var x797 int32 = mtmp796._v0_0
        jp883 = x797
        var t884 int32
        var inline944 int32 = 1
        var inline945 int32 = jp883 + inline944
        t884 = inline945
        var t885 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: t884,
        }
        return t885
    case 1:
        var x798 string = mtmp796._v1_0
        var t886 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t886
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
