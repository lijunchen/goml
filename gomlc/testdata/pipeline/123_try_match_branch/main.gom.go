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

type Ordering int32

type Choice struct {
    _tag int32
    _v0_0 bool
    _v1_0 bool
    _v2_0 int32
}

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func choose(choice__2 Choice) Result__i32__string {
    var jp827 int32
    switch choice__2._tag {
    case 0:
        var x796 bool = choice__2._v0_0
        var commute_field979 int32
        var commute_field981 string
        if x796 {
            commute_field979 = 10
            jp827 = commute_field979
            var t828 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: jp827,
            }
            return t828
        } else {
            commute_field981 = "left failed"
            var t831 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: commute_field981,
            }
            return t831
        }
    case 1:
        var x797 bool = choice__2._v1_0
        var mtmp802 Result__i32__string
        if x797 {
            var inline914 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: 20,
            }
            mtmp802 = inline914
        } else {
            var inline915 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: "right failed",
            }
            mtmp802 = inline915
        }
        var jp833 int32
        switch mtmp802._tag {
        case 0:
            var x803 int32 = mtmp802._v0_0
            jp833 = x803
            var t834 int32 = jp833 + 1
            jp827 = t834
            var t828 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: jp827,
            }
            return t828
        case 1:
            var x804 string = mtmp802._v1_0
            var t835 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: x804,
            }
            return t835
        default:
            panic("non-exhaustive match")
        }
    case 2:
        var x798 int32 = choice__2._v2_0
        jp827 = x798
        var t828 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: jp827,
        }
        return t828
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__i32__string) string {
    switch res__7._tag {
    case 0:
        var x805 int32 = res__7._v0_0
        var t840 string
        var inline917 string = __goml_builtin_int32_to_string(x805)
        t840 = inline917
        var t841 string = "ok " + t840
        return t841
    case 1:
        var x806 string = res__7._v1_0
        var t842 string = "err " + x806
        return t842
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t844 Choice = Choice{
        _tag: 0,
        _v0_0: true,
    }
    var t845 Result__i32__string = choose(t844)
    var t846 string = show(t845)
    var inline955 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t846)
    _goml_runtime_core_string_println(inline955)
    var t847 Choice = Choice{
        _tag: 1,
        _v1_0: true,
    }
    var t848 Result__i32__string = choose(t847)
    var t849 string = show(t848)
    var inline952 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t849)
    _goml_runtime_core_string_println(inline952)
    var t850 Choice = Choice{
        _tag: 2,
        _v2_0: 5,
    }
    var t851 Result__i32__string = choose(t850)
    var t852 string
    switch t851._tag {
    case 0:
        var inline944 int32 = t851._v0_0
        var inline946 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline944)
        var inline947 string = "ok " + inline946
        t852 = inline947
    case 1:
        var inline948 string = t851._v1_0
        var inline950 string = "err " + inline948
        t852 = inline950
    default:
        panic("non-exhaustive match")
    }
    var inline941 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t852)
    _goml_runtime_core_string_println(inline941)
    var t853 Choice = Choice{
        _tag: 0,
        _v0_0: false,
    }
    var t854 Result__i32__string = choose(t853)
    var t855 string
    switch t854._tag {
    case 0:
        var inline933 int32 = t854._v0_0
        var inline935 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline933)
        var inline936 string = "ok " + inline935
        t855 = inline936
    case 1:
        var inline937 string = t854._v1_0
        var inline939 string = "err " + inline937
        t855 = inline939
    default:
        panic("non-exhaustive match")
    }
    var inline930 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t855)
    _goml_runtime_core_string_println(inline930)
    var t856 Choice = Choice{
        _tag: 1,
        _v1_0: false,
    }
    var t857 Result__i32__string = choose(t856)
    var t858 string
    switch t857._tag {
    case 0:
        var inline922 int32 = t857._v0_0
        var inline924 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline922)
        var inline925 string = "ok " + inline924
        t858 = inline925
    case 1:
        var inline926 string = t857._v1_0
        var inline928 string = "err " + inline926
        t858 = inline928
    default:
        panic("non-exhaustive match")
    }
    var inline919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t858)
    _goml_runtime_core_string_println(inline919)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline958 int64 = int64(int32(self__286))
    var inline959 string = signed_decimal_string(inline958)
    return inline959
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t867 int64 = int64(int32(value__225))
    var inline962 bool = t867 < 0
    if inline962 {
        var inline963 uint64 = uint64(int64(t867))
        var inline964 uint64 = 0 - inline963
        var inline965 string = decimal_string(inline964)
        var inline966 string = "-" + inline965
        return inline966
    } else {
        var inline967 uint64 = uint64(int64(t867))
        var inline968 string = decimal_string(inline967)
        return inline968
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t875 bool = value__214 < 0
    if t875 {
        var t876 uint64 = uint64(int64(value__214))
        var t877 uint64 = 0 - t876
        var t878 string = decimal_string(t877)
        var t879 string = "-" + t878
        return t879
    } else {
        var t880 uint64 = uint64(int64(value__214))
        var t881 string = decimal_string(t880)
        return t881
    }
}

func decimal_string(value__208 uint64) string {
    var t904 bool = value__208 == 0
    if t904 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop897:
        for {
            var t898 bool = remaining__210 > 0
            if t898 {
                var t899_rhs uint64 = 10
                var t899 uint64 = remaining__210 % t899_rhs
                var t900 uint8 = uint8(uint64(t899))
                var t901 uint8 = t900 + 48
                vec_push__Vec_5uint8(reversed__209, t901)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t902 uint64 = compound_old353 / compound_value354
                remaining__210 = t902
                continue
            } else {
                break Loop_loop897
            }
        }
        var t886 int
        var inline978 int = vec_len__Vec_5uint8(reversed__209)
        t886 = inline978
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t886)
        var offset__212 int = 0
        Loop_loop888:
        for {
            var t889 int
            var inline976 int = vec_len__Vec_5uint8(reversed__209)
            t889 = inline976
            var t890 bool = offset__212 < t889
            if t890 {
                var t891 int
                var inline974 int = vec_len__Vec_5uint8(reversed__209)
                t891 = inline974
                var t892 int = t891 - offset__212
                var t893 int = t892 - 1
                var t894 uint8 = vec_get__Vec_5uint8(reversed__209, t893)
                vec_push__Vec_5uint8(bytes__211, t894)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t895 int = compound_old358 + compound_value359
                offset__212 = t895
                continue
            } else {
                break Loop_loop888
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
