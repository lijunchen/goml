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

type NumberIterator struct {}

type Numbers struct {}

type Ordering int32

func main0() struct{} {
    var t799 int32
    var inline856 int32 = 42
    t799 = inline856
    var inline853 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t799)
    _goml_runtime_core_string_println(inline853)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline860 int64 = int64(int32(self__407))
    var inline861 string = signed_decimal_string(inline860)
    return inline861
}

func signed_decimal_string(value__214 int64) string {
    var t817 bool = value__214 < 0
    if t817 {
        var t818 uint64 = uint64(int64(value__214))
        var t819 uint64 = 0 - t818
        var t820 string = decimal_string(t819)
        var t821 string = "-" + t820
        return t821
    } else {
        var t822 uint64 = uint64(int64(value__214))
        var t823 string = decimal_string(t822)
        return t823
    }
}

func decimal_string(value__208 uint64) string {
    var t846 bool = value__208 == 0
    if t846 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop839:
        for {
            var t840 bool = remaining__210 > 0
            if t840 {
                var t841_rhs uint64 = 10
                var t841 uint64 = remaining__210 % t841_rhs
                var t842 uint8 = uint8(uint64(t841))
                var t843 uint8 = t842 + 48
                vec_push__Vec_5uint8(reversed__209, t843)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t844 uint64 = compound_old353 / compound_value354
                remaining__210 = t844
                continue
            } else {
                break Loop_loop839
            }
        }
        var t828 int
        var inline879 int = vec_len__Vec_5uint8(reversed__209)
        t828 = inline879
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t828)
        var offset__212 int = 0
        Loop_loop830:
        for {
            var t831 int
            var inline877 int = vec_len__Vec_5uint8(reversed__209)
            t831 = inline877
            var t832 bool = offset__212 < t831
            if t832 {
                var t833 int
                var inline875 int = vec_len__Vec_5uint8(reversed__209)
                t833 = inline875
                var t834 int = t833 - offset__212
                var t835 int = t834 - 1
                var t836 uint8 = vec_get__Vec_5uint8(reversed__209, t835)
                vec_push__Vec_5uint8(bytes__211, t836)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t837 int = compound_old358 + compound_value359
                offset__212 = t837
                continue
            } else {
                break Loop_loop830
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
