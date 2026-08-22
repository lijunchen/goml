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

type UserId struct {
    _0 int32
}

type Box__string struct {
    _0 string
}

type Ordering int32

func main0() struct{} {
    var raw__4 int32 = 40
    var id__5 UserId = UserId{
        _0: raw__4,
    }
    var inline892 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(raw__4)
    _goml_runtime_core_string_println(inline892)
    var t815 UserId
    var inline883 UserId = id__5
    var inline884 UserId = inline883
    var inline885 int32 = inline884._0
    var inline886 int32 = 1
    var inline887 int32 = inline885 + inline886
    var inline888 UserId = UserId{
        _0: inline887,
    }
    inline883 = inline888
    t815 = inline883
    var t816 int32
    var inline880 int32 = t815._0
    t816 = inline880
    var inline876 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t816)
    _goml_runtime_core_string_println(inline876)
    var x805 string = "wrapped"
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x805)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline898 int64 = int64(int32(self__407))
    var inline899 string = signed_decimal_string(inline898)
    return inline899
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t837 bool = value__214 < 0
    if t837 {
        var t838 uint64 = uint64(int64(value__214))
        var t839 uint64 = 0 - t838
        var t840 string = decimal_string(t839)
        var t841 string = "-" + t840
        return t841
    } else {
        var t842 uint64 = uint64(int64(value__214))
        var t843 string = decimal_string(t842)
        return t843
    }
}

func decimal_string(value__208 uint64) string {
    var t866 bool = value__208 == 0
    if t866 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop859:
        for {
            var t860 bool = remaining__210 > 0
            if t860 {
                var t861_rhs uint64 = 10
                var t861 uint64 = remaining__210 % t861_rhs
                var t862 uint8 = uint8(uint64(t861))
                var t863 uint8 = t862 + 48
                vec_push__Vec_5uint8(reversed__209, t863)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t864 uint64 = compound_old353 / compound_value354
                remaining__210 = t864
                continue
            } else {
                break Loop_loop859
            }
        }
        var t848 int
        var inline917 int = vec_len__Vec_5uint8(reversed__209)
        t848 = inline917
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t848)
        var offset__212 int = 0
        Loop_loop850:
        for {
            var t851 int
            var inline915 int = vec_len__Vec_5uint8(reversed__209)
            t851 = inline915
            var t852 bool = offset__212 < t851
            if t852 {
                var t853 int
                var inline913 int = vec_len__Vec_5uint8(reversed__209)
                t853 = inline913
                var t854 int = t853 - offset__212
                var t855 int = t854 - 1
                var t856 uint8 = vec_get__Vec_5uint8(reversed__209, t855)
                vec_push__Vec_5uint8(bytes__211, t856)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t857 int = compound_old358 + compound_value359
                offset__212 = t857
                continue
            } else {
                break Loop_loop850
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
