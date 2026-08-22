package main

import (
    _goml_os "os"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
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

func main0() struct{} {
    var inline875 struct{} = struct{}{}
    var inline876 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline875)
    _goml_runtime_core_string_print(inline876)
    var inline871 bool = true
    var inline872 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline871)
    _goml_runtime_core_string_print(inline872)
    var inline867 bool = false
    var inline868 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline867)
    _goml_runtime_core_string_print(inline868)
    var inline863 int = 123
    var inline864 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline863)
    _goml_runtime_core_string_print(inline864)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__400 struct{}) string {
    var t812 string = _goml_runtime_core_unit_to_string(self__400)
    return t812
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t815 string = _goml_runtime_core_bool_to_string(self__401)
    return t815
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline885 int64 = int64(int(self__404))
    var inline886 string = signed_decimal_string(inline885)
    return inline886
}

func signed_decimal_string(value__214 int64) string {
    var t827 bool = value__214 < 0
    if t827 {
        var t828 uint64 = uint64(int64(value__214))
        var t829 uint64 = 0 - t828
        var t830 string = decimal_string(t829)
        var t831 string = "-" + t830
        return t831
    } else {
        var t832 uint64 = uint64(int64(value__214))
        var t833 string = decimal_string(t832)
        return t833
    }
}

func decimal_string(value__208 uint64) string {
    var t856 bool = value__208 == 0
    if t856 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop849:
        for {
            var t850 bool = remaining__210 > 0
            if t850 {
                var t851_rhs uint64 = 10
                var t851 uint64 = remaining__210 % t851_rhs
                var t852 uint8 = uint8(uint64(t851))
                var t853 uint8 = t852 + 48
                vec_push__Vec_5uint8(reversed__209, t853)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t854 uint64 = compound_old353 / compound_value354
                remaining__210 = t854
                continue
            } else {
                break Loop_loop849
            }
        }
        var t838 int
        var inline904 int = vec_len__Vec_5uint8(reversed__209)
        t838 = inline904
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t838)
        var offset__212 int = 0
        Loop_loop840:
        for {
            var t841 int
            var inline902 int = vec_len__Vec_5uint8(reversed__209)
            t841 = inline902
            var t842 bool = offset__212 < t841
            if t842 {
                var t843 int
                var inline900 int = vec_len__Vec_5uint8(reversed__209)
                t843 = inline900
                var t844 int = t843 - offset__212
                var t845 int = t844 - 1
                var t846 uint8 = vec_get__Vec_5uint8(reversed__209, t845)
                vec_push__Vec_5uint8(bytes__211, t846)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t847 int = compound_old358 + compound_value359
                offset__212 = t847
                continue
            } else {
                break Loop_loop840
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
