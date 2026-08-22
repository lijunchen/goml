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

func main0() struct{} {
    var start8__0 uint8 = 200
    var add8__1 uint8 = 55
    var sum8__2 uint8 = start8__0 + add8__1
    var neg8__3 uint8 = -start8__0
    var start16__4 uint16 = 50000
    var add16__5 uint16 = 12000
    var sum16__6 uint16 = start16__4 + add16__5
    var diff16__7 uint16 = sum16__6 - start16__4
    var add32__9 uint32 = 123456789
    var neg32__11 uint32 = -add32__9
    var start64__12 uint64 = 6000000000
    var add64__13 uint64 = 4000000000
    var sum64__14 uint64 = start64__12 + add64__13
    var diff64__15 uint64 = sum64__14 - add64__13
    var t798 string
    var inline882 string = __goml_builtin_uint8_to_string(sum8__2)
    t798 = inline882
    var t799 string = t798 + ", "
    var t800 string
    var inline880 string = __goml_builtin_uint8_to_string(neg8__3)
    t800 = inline880
    var t801 string = t799 + t800
    var t802 string = t801 + "; "
    var t803 string
    var inline878 string = __goml_builtin_uint16_to_string(diff16__7)
    t803 = inline878
    var t804 string = t802 + t803
    var t805 string = t804 + "; "
    var t806 string
    var inline876 string = __goml_builtin_uint32_to_string(neg32__11)
    t806 = inline876
    var t807 string = t805 + t806
    var t808 string = t807 + "; "
    var t809 string
    var inline874 string = __goml_builtin_uint64_to_string(diff64__15)
    t809 = inline874
    var message__16 string = t808 + t809
    var inline871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline871)
    return struct{}{}
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t827 uint64 = uint64(uint8(value__228))
    var t828 string = decimal_string(t827)
    return t828
}

func __goml_builtin_uint16_to_string(value__229 uint16) string {
    var t831 uint64 = uint64(uint16(value__229))
    var t832 string = decimal_string(t831)
    return t832
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t835 uint64 = uint64(uint32(value__230))
    var t836 string = decimal_string(t835)
    return t836
}

func __goml_builtin_uint64_to_string(value__231 uint64) string {
    var t839 string = decimal_string(value__231)
    return t839
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t864 bool = value__208 == 0
    if t864 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop857:
        for {
            var t858 bool = remaining__210 > 0
            if t858 {
                var t859_rhs uint64 = 10
                var t859 uint64 = remaining__210 % t859_rhs
                var t860 uint8 = uint8(uint64(t859))
                var t861 uint8 = t860 + 48
                vec_push__Vec_5uint8(reversed__209, t861)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t862 uint64 = compound_old353 / compound_value354
                remaining__210 = t862
                continue
            } else {
                break Loop_loop857
            }
        }
        var t846 int
        var inline904 int = vec_len__Vec_5uint8(reversed__209)
        t846 = inline904
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t846)
        var offset__212 int = 0
        Loop_loop848:
        for {
            var t849 int
            var inline902 int = vec_len__Vec_5uint8(reversed__209)
            t849 = inline902
            var t850 bool = offset__212 < t849
            if t850 {
                var t851 int
                var inline900 int = vec_len__Vec_5uint8(reversed__209)
                t851 = inline900
                var t852 int = t851 - offset__212
                var t853 int = t852 - 1
                var t854 uint8 = vec_get__Vec_5uint8(reversed__209, t853)
                vec_push__Vec_5uint8(bytes__211, t854)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t855 int = compound_old358 + compound_value359
                offset__212 = t855
                continue
            } else {
                break Loop_loop848
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
