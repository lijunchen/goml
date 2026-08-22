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

type S struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t804 string
    var inline862 int32 = 7
    var inline863 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline862)
    var inline864 string = "S(" + inline863
    var inline865 string = inline864 + ")"
    t804 = inline865
    var inline859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t804)
    _goml_runtime_core_string_println(inline859)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline867 int64 = int64(int32(self__286))
    var inline868 string = signed_decimal_string(inline867)
    return inline868
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t821 bool = value__214 < 0
    if t821 {
        var t822 uint64 = uint64(int64(value__214))
        var t823 uint64 = 0 - t822
        var t824 string = decimal_string(t823)
        var t825 string = "-" + t824
        return t825
    } else {
        var t826 uint64 = uint64(int64(value__214))
        var t827 string = decimal_string(t826)
        return t827
    }
}

func decimal_string(value__208 uint64) string {
    var t850 bool = value__208 == 0
    if t850 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop843:
        for {
            var t844 bool = remaining__210 > 0
            if t844 {
                var t845_rhs uint64 = 10
                var t845 uint64 = remaining__210 % t845_rhs
                var t846 uint8 = uint8(uint64(t845))
                var t847 uint8 = t846 + 48
                vec_push__Vec_5uint8(reversed__209, t847)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t848 uint64 = compound_old353 / compound_value354
                remaining__210 = t848
                continue
            } else {
                break Loop_loop843
            }
        }
        var t832 int
        var inline887 int = vec_len__Vec_5uint8(reversed__209)
        t832 = inline887
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t832)
        var offset__212 int = 0
        Loop_loop834:
        for {
            var t835 int
            var inline885 int = vec_len__Vec_5uint8(reversed__209)
            t835 = inline885
            var t836 bool = offset__212 < t835
            if t836 {
                var t837 int
                var inline883 int = vec_len__Vec_5uint8(reversed__209)
                t837 = inline883
                var t838 int = t837 - offset__212
                var t839 int = t838 - 1
                var t840 uint8 = vec_get__Vec_5uint8(reversed__209, t839)
                vec_push__Vec_5uint8(bytes__211, t840)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t841 int = compound_old358 + compound_value359
                offset__212 = t841
                continue
            } else {
                break Loop_loop834
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
