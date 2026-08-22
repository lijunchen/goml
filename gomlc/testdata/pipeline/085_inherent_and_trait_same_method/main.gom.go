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

type Boxed struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t806 string
    t806 = "inherent"
    var inline869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline869)
    var t808 string
    var inline866 int32 = 9
    var inline867 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline866)
    t808 = inline867
    var inline863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline863)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline873 int64 = int64(int32(self__286))
    var inline874 string = signed_decimal_string(inline873)
    return inline874
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t825 bool = value__214 < 0
    if t825 {
        var t826 uint64 = uint64(int64(value__214))
        var t827 uint64 = 0 - t826
        var t828 string = decimal_string(t827)
        var t829 string = "-" + t828
        return t829
    } else {
        var t830 uint64 = uint64(int64(value__214))
        var t831 string = decimal_string(t830)
        return t831
    }
}

func decimal_string(value__208 uint64) string {
    var t854 bool = value__208 == 0
    if t854 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop847:
        for {
            var t848 bool = remaining__210 > 0
            if t848 {
                var t849_rhs uint64 = 10
                var t849 uint64 = remaining__210 % t849_rhs
                var t850 uint8 = uint8(uint64(t849))
                var t851 uint8 = t850 + 48
                vec_push__Vec_5uint8(reversed__209, t851)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t852 uint64 = compound_old353 / compound_value354
                remaining__210 = t852
                continue
            } else {
                break Loop_loop847
            }
        }
        var t836 int
        var inline893 int = vec_len__Vec_5uint8(reversed__209)
        t836 = inline893
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t836)
        var offset__212 int = 0
        Loop_loop838:
        for {
            var t839 int
            var inline891 int = vec_len__Vec_5uint8(reversed__209)
            t839 = inline891
            var t840 bool = offset__212 < t839
            if t840 {
                var t841 int
                var inline889 int = vec_len__Vec_5uint8(reversed__209)
                t841 = inline889
                var t842 int = t841 - offset__212
                var t843 int = t842 - 1
                var t844 uint8 = vec_get__Vec_5uint8(reversed__209, t843)
                vec_push__Vec_5uint8(bytes__211, t844)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t845 int = compound_old358 + compound_value359
                offset__212 = t845
                continue
            } else {
                break Loop_loop838
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
