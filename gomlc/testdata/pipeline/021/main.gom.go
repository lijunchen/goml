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

func match_int(n__0 int32) int32 {
    switch n__0 {
    case 0:
        return 10
    case 1:
        return 20
    default:
        return 30
    }
}

func main0() struct{} {
    var t818 int32 = match_int(0)
    var t819 string = _goml_m_inherent_i_i32_i_i32_i_to__string(t818)
    println__T_string(t819)
    var t820 int32 = match_int(5)
    var t821 string
    var inline929 string = __goml_builtin_int32_to_string(t820)
    t821 = inline929
    println__T_string(t821)
    var t822 int32
    t822 = 40
    var t823 string
    var inline925 string = __goml_builtin_int32_to_string(t822)
    t823 = inline925
    var inline922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline922)
    var t824 int32
    t824 = 40
    var t825 string
    var inline918 string = __goml_builtin_int32_to_string(t824)
    t825 = inline918
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
    _goml_runtime_core_string_println(inline915)
    var t826 int32
    var inline913 int32 = 2
    switch inline913 {
    case 2:
        t826 = 90
    case 3:
        t826 = 100
    default:
        t826 = 100
    }
    var t827 string
    var inline911 string = __goml_builtin_int32_to_string(t826)
    t827 = inline911
    var inline908 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t827)
    _goml_runtime_core_string_println(inline908)
    var t828 int32
    var inline906 int32 = 3
    switch inline906 {
    case 2:
        t828 = 90
    case 3:
        t828 = 100
    default:
        t828 = 100
    }
    var t829 string
    var inline904 string = __goml_builtin_int32_to_string(t828)
    t829 = inline904
    var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
    _goml_runtime_core_string_println(inline901)
    var t830 int32
    var inline899 int32 = 1
    switch inline899 {
    case 1:
        t830 = 60
    default:
        t830 = 80
    }
    var t831 string
    var inline897 string = __goml_builtin_int32_to_string(t830)
    t831 = inline897
    var inline894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline894)
    var t832 int32
    var inline892 int32 = 3
    switch inline892 {
    case 1:
        t832 = 60
    default:
        t832 = 80
    }
    var t833 string
    var inline890 string = __goml_builtin_int32_to_string(t832)
    t833 = inline890
    var inline887 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t833)
    _goml_runtime_core_string_println(inline887)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t836 string
    t836 = value__1
    _goml_runtime_core_string_println(t836)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline932 int64 = int64(int32(self__286))
    var inline933 string = signed_decimal_string(inline932)
    return inline933
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t845 int64 = int64(int32(value__225))
    var inline935 bool = t845 < 0
    if inline935 {
        var inline936 uint64 = uint64(int64(t845))
        var inline937 uint64 = 0 - inline936
        var inline938 string = decimal_string(inline937)
        var inline939 string = "-" + inline938
        return inline939
    } else {
        var inline940 uint64 = uint64(int64(t845))
        var inline941 string = decimal_string(inline940)
        return inline941
    }
}

func signed_decimal_string(value__214 int64) string {
    var t851 bool = value__214 < 0
    if t851 {
        var t852 uint64 = uint64(int64(value__214))
        var t853 uint64 = 0 - t852
        var t854 string = decimal_string(t853)
        var t855 string = "-" + t854
        return t855
    } else {
        var t856 uint64 = uint64(int64(value__214))
        var t857 string = decimal_string(t856)
        return t857
    }
}

func decimal_string(value__208 uint64) string {
    var t880 bool = value__208 == 0
    if t880 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop873:
        for {
            var t874 bool = remaining__210 > 0
            if t874 {
                var t875_rhs uint64 = 10
                var t875 uint64 = remaining__210 % t875_rhs
                var t876 uint8 = uint8(uint64(t875))
                var t877 uint8 = t876 + 48
                vec_push__Vec_5uint8(reversed__209, t877)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t878 uint64 = compound_old353 / compound_value354
                remaining__210 = t878
                continue
            } else {
                break Loop_loop873
            }
        }
        var t862 int
        var inline951 int = vec_len__Vec_5uint8(reversed__209)
        t862 = inline951
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t862)
        var offset__212 int = 0
        Loop_loop864:
        for {
            var t865 int
            var inline949 int = vec_len__Vec_5uint8(reversed__209)
            t865 = inline949
            var t866 bool = offset__212 < t865
            if t866 {
                var t867 int
                var inline947 int = vec_len__Vec_5uint8(reversed__209)
                t867 = inline947
                var t868 int = t867 - offset__212
                var t869 int = t868 - 1
                var t870 uint8 = vec_get__Vec_5uint8(reversed__209, t869)
                vec_push__Vec_5uint8(bytes__211, t870)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t871 int = compound_old358 + compound_value359
                offset__212 = t871
                continue
            } else {
                break Loop_loop864
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
