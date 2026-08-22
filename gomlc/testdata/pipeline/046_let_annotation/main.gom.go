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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
    return struct{}{}
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

func main0() struct{} {
    var x__0 int32 = 1
    var y__1 int8 = 1
    var inline876 string = "i32: "
    var inline877 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline876)
    _goml_runtime_core_string_print(inline877)
    var inline873 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline873)
    var inline869 string = "i8: "
    var inline870 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline869)
    _goml_runtime_core_string_print(inline870)
    var inline866 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline866)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline885 int64 = int64(int32(self__407))
    var inline886 string = signed_decimal_string(inline885)
    return inline886
}

func _goml_m_trait__impl_i_ToString_i_i8_i_to__string(self__405 int8) string {
    var inline888 int64 = int64(int8(self__405))
    var inline889 string = signed_decimal_string(inline888)
    return inline889
}

func signed_decimal_string(value__214 int64) string {
    var t830 bool = value__214 < 0
    if t830 {
        var t831 uint64 = uint64(int64(value__214))
        var t832 uint64 = 0 - t831
        var t833 string = decimal_string(t832)
        var t834 string = "-" + t833
        return t834
    } else {
        var t835 uint64 = uint64(int64(value__214))
        var t836 string = decimal_string(t835)
        return t836
    }
}

func decimal_string(value__208 uint64) string {
    var t859 bool = value__208 == 0
    if t859 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop852:
        for {
            var t853 bool = remaining__210 > 0
            if t853 {
                var t854_rhs uint64 = 10
                var t854 uint64 = remaining__210 % t854_rhs
                var t855 uint8 = uint8(uint64(t854))
                var t856 uint8 = t855 + 48
                vec_push__Vec_5uint8(reversed__209, t856)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t857 uint64 = compound_old353 / compound_value354
                remaining__210 = t857
                continue
            } else {
                break Loop_loop852
            }
        }
        var t841 int
        var inline915 int = vec_len__Vec_5uint8(reversed__209)
        t841 = inline915
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t841)
        var offset__212 int = 0
        Loop_loop843:
        for {
            var t844 int
            var inline913 int = vec_len__Vec_5uint8(reversed__209)
            t844 = inline913
            var t845 bool = offset__212 < t844
            if t845 {
                var t846 int
                var inline911 int = vec_len__Vec_5uint8(reversed__209)
                t846 = inline911
                var t847 int = t846 - offset__212
                var t848 int = t847 - 1
                var t849 uint8 = vec_get__Vec_5uint8(reversed__209, t848)
                vec_push__Vec_5uint8(bytes__211, t849)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t850 int = compound_old358 + compound_value359
                offset__212 = t850
                continue
            } else {
                break Loop_loop843
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
