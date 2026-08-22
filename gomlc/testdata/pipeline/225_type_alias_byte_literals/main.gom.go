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

type Tuple2_5int32_5int32 struct {
    _0 int32
    _1 int32
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

func main0() struct{} {
    var user__2 uint64 = 255
    var marker__4 uint8 = 65
    var inline923 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline923)
    var t818 int32
    var inline919 int32 = 10
    var inline920 int32 = 10
    var inline921 int32 = inline919 + inline920
    t818 = inline921
    var inline916 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t818)
    _goml_runtime_core_string_println(inline916)
    var inline913 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline913)
    var t819 string
    var inline909 bool = marker__4 == 10
    if inline909 {
        t819 = "newline"
    } else {
        var inline910 bool = marker__4 >= 65
        if inline910 {
            var inline911 bool = marker__4 <= 90
            if inline911 {
                t819 = "uppercase"
            } else {
                t819 = "other"
            }
        } else {
            t819 = "other"
        }
    }
    var inline905 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t819)
    _goml_runtime_core_string_println(inline905)
    var t820 string
    var inline899 uint8 = 10
    var inline901 bool = inline899 == 10
    if inline901 {
        t820 = "newline"
    } else {
        var inline902 bool = inline899 >= 65
        if inline902 {
            var inline903 bool = inline899 <= 90
            if inline903 {
                t820 = "uppercase"
            } else {
                t820 = "other"
            }
        } else {
            t820 = "other"
        }
    }
    var inline896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline896)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__412 uint64) string {
    var inline933 string = decimal_string(self__412)
    return inline933
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline935 int64 = int64(int32(self__407))
    var inline936 string = signed_decimal_string(inline935)
    return inline936
}

func _goml_m_trait__impl_i_ToString_i_u8_i_to__string(self__409 uint8) string {
    var inline938 uint64 = uint64(uint8(self__409))
    var inline939 string = decimal_string(inline938)
    return inline939
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t878 bool = value__208 == 0
    if t878 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop871:
        for {
            var t872 bool = remaining__210 > 0
            if t872 {
                var t873_rhs uint64 = 10
                var t873 uint64 = remaining__210 % t873_rhs
                var t874 uint8 = uint8(uint64(t873))
                var t875 uint8 = t874 + 48
                vec_push__Vec_5uint8(reversed__209, t875)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t876 uint64 = compound_old353 / compound_value354
                remaining__210 = t876
                continue
            } else {
                break Loop_loop871
            }
        }
        var t860 int
        var inline957 int = vec_len__Vec_5uint8(reversed__209)
        t860 = inline957
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t860)
        var offset__212 int = 0
        Loop_loop862:
        for {
            var t863 int
            var inline955 int = vec_len__Vec_5uint8(reversed__209)
            t863 = inline955
            var t864 bool = offset__212 < t863
            if t864 {
                var t865 int
                var inline953 int = vec_len__Vec_5uint8(reversed__209)
                t865 = inline953
                var t866 int = t865 - offset__212
                var t867 int = t866 - 1
                var t868 uint8 = vec_get__Vec_5uint8(reversed__209, t867)
                vec_push__Vec_5uint8(bytes__211, t868)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t869 int = compound_old358 + compound_value359
                offset__212 = t869
                continue
            } else {
                break Loop_loop862
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func signed_decimal_string(value__214 int64) string {
    var t883 bool = value__214 < 0
    if t883 {
        var t884 uint64 = uint64(int64(value__214))
        var t885 uint64 = 0 - t884
        var t886 string = decimal_string(t885)
        var t887 string = "-" + t886
        return t887
    } else {
        var t888 uint64 = uint64(int64(value__214))
        var t889 string = decimal_string(t888)
        return t889
    }
}

func main() {
    main0()
}
