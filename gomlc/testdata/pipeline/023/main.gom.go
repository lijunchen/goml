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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
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

type Mixed interface {
    isMixed()
}

type OnlyInt struct {
    _0 int32
}

func (_ OnlyInt) isMixed() {}

type OnlyStr struct {
    _0 string
}

func (_ OnlyStr) isMixed() {}

type Both struct {
    _0 int32
    _1 string
}

func (_ Both) isMixed() {}

func match_mixed_pair(pair__0 Tuple2_5int32_6string) int32 {
    var x796 int32 = pair__0._0
    var x797 string = pair__0._1
    switch x797 {
    case "zero":
        switch x796 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x796 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x796 {
        case 0:
            return 2
        default:
            return 5
        }
    }
}

func match_mixed_enum(value__1 Mixed) int32 {
    switch value__1.(type) {
    case OnlyInt:
        var x798 int32 = value__1.(OnlyInt)._0
        switch x798 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x799 string = value__1.(OnlyStr)._0
        switch x799 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x800 int32 = value__1.(Both)._0
        var x801 string = value__1.(Both)._1
        switch x801 {
        case "zero":
            switch x800 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x800 {
            case 0:
                return 11
            default:
                return 13
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t839 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t840 int32 = match_mixed_pair(t839)
    var t841 string = _goml_m_inherent_i_i32_i_i32_i_to__string(t840)
    println__T_string(t841)
    var t842 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t843 int32 = match_mixed_pair(t842)
    var t844 string = _goml_m_inherent_i_i32_i_i32_i_to__string(t843)
    println__T_string(t844)
    var t845 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t846 int32 = match_mixed_pair(t845)
    var t847 string = _goml_m_inherent_i_i32_i_i32_i_to__string(t846)
    println__T_string(t847)
    var t848 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t849 int32 = match_mixed_pair(t848)
    var t850 string = _goml_m_inherent_i_i32_i_i32_i_to__string(t849)
    var inline976 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t850)
    _goml_runtime_core_string_println(inline976)
    var t851 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t852 int32 = match_mixed_pair(t851)
    var t853 string
    var inline974 string = __goml_builtin_int32_to_string(t852)
    t853 = inline974
    var inline971 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t853)
    _goml_runtime_core_string_println(inline971)
    var t854 Mixed = OnlyInt{
        _0: 0,
    }
    var t855 int32 = match_mixed_enum(t854)
    var t856 string
    var inline969 string = __goml_builtin_int32_to_string(t855)
    t856 = inline969
    var inline966 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t856)
    _goml_runtime_core_string_println(inline966)
    var t857 Mixed = OnlyInt{
        _0: 5,
    }
    var t858 int32 = match_mixed_enum(t857)
    var t859 string
    var inline964 string = __goml_builtin_int32_to_string(t858)
    t859 = inline964
    var inline961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t859)
    _goml_runtime_core_string_println(inline961)
    var t860 Mixed = OnlyStr{
        _0: "zero",
    }
    var t861 int32 = match_mixed_enum(t860)
    var t862 string
    var inline959 string = __goml_builtin_int32_to_string(t861)
    t862 = inline959
    var inline956 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t862)
    _goml_runtime_core_string_println(inline956)
    var t863 Mixed = OnlyStr{
        _0: "hello",
    }
    var t864 int32 = match_mixed_enum(t863)
    var t865 string
    var inline954 string = __goml_builtin_int32_to_string(t864)
    t865 = inline954
    var inline951 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t865)
    _goml_runtime_core_string_println(inline951)
    var t866 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t867 int32 = match_mixed_enum(t866)
    var t868 string
    var inline949 string = __goml_builtin_int32_to_string(t867)
    t868 = inline949
    var inline946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t868)
    _goml_runtime_core_string_println(inline946)
    var t869 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t870 int32 = match_mixed_enum(t869)
    var t871 string
    var inline944 string = __goml_builtin_int32_to_string(t870)
    t871 = inline944
    var inline941 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t871)
    _goml_runtime_core_string_println(inline941)
    var t872 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t873 int32 = match_mixed_enum(t872)
    var t874 string
    var inline939 string = __goml_builtin_int32_to_string(t873)
    t874 = inline939
    var inline936 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t874)
    _goml_runtime_core_string_println(inline936)
    var t875 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t876 int32 = match_mixed_enum(t875)
    var t877 string
    var inline934 string = __goml_builtin_int32_to_string(t876)
    t877 = inline934
    var inline931 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t877)
    _goml_runtime_core_string_println(inline931)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t880 string
    t880 = value__1
    _goml_runtime_core_string_println(t880)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline980 int64 = int64(int32(self__286))
    var inline981 string = signed_decimal_string(inline980)
    return inline981
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t889 int64 = int64(int32(value__225))
    var inline983 bool = t889 < 0
    if inline983 {
        var inline984 uint64 = uint64(int64(t889))
        var inline985 uint64 = 0 - inline984
        var inline986 string = decimal_string(inline985)
        var inline987 string = "-" + inline986
        return inline987
    } else {
        var inline988 uint64 = uint64(int64(t889))
        var inline989 string = decimal_string(inline988)
        return inline989
    }
}

func signed_decimal_string(value__214 int64) string {
    var t895 bool = value__214 < 0
    if t895 {
        var t896 uint64 = uint64(int64(value__214))
        var t897 uint64 = 0 - t896
        var t898 string = decimal_string(t897)
        var t899 string = "-" + t898
        return t899
    } else {
        var t900 uint64 = uint64(int64(value__214))
        var t901 string = decimal_string(t900)
        return t901
    }
}

func decimal_string(value__208 uint64) string {
    var t924 bool = value__208 == 0
    if t924 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop917:
        for {
            var t918 bool = remaining__210 > 0
            if t918 {
                var t919_rhs uint64 = 10
                var t919 uint64 = remaining__210 % t919_rhs
                var t920 uint8 = uint8(uint64(t919))
                var t921 uint8 = t920 + 48
                vec_push__Vec_5uint8(reversed__209, t921)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t922 uint64 = compound_old353 / compound_value354
                remaining__210 = t922
                continue
            } else {
                break Loop_loop917
            }
        }
        var t906 int
        var inline999 int = vec_len__Vec_5uint8(reversed__209)
        t906 = inline999
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t906)
        var offset__212 int = 0
        Loop_loop908:
        for {
            var t909 int
            var inline997 int = vec_len__Vec_5uint8(reversed__209)
            t909 = inline997
            var t910 bool = offset__212 < t909
            if t910 {
                var t911 int
                var inline995 int = vec_len__Vec_5uint8(reversed__209)
                t911 = inline995
                var t912 int = t911 - offset__212
                var t913 int = t912 - 1
                var t914 uint8 = vec_get__Vec_5uint8(reversed__209, t913)
                vec_push__Vec_5uint8(bytes__211, t914)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t915 int = compound_old358 + compound_value359
                offset__212 = t915
                continue
            } else {
                break Loop_loop908
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
