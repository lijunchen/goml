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

func fib(x__0 int32) int32 {
    var mtmp796 bool = x__0 < 2
    switch mtmp796 {
    case true:
        return 1
    case false:
        var t802 int32 = x__0 - 1
        var t803 int32 = fib(t802)
        var t804 int32 = x__0 - 2
        var t805 int32 = fib(t804)
        var t806 int32 = t803 + t805
        return t806
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t808 int32 = fib(10)
    var inline859 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t808)
    _goml_runtime_core_string_print(inline859)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline864 int64 = int64(int32(self__407))
    var inline865 string = signed_decimal_string(inline864)
    return inline865
}

func signed_decimal_string(value__214 int64) string {
    var t823 bool = value__214 < 0
    if t823 {
        var t824 uint64 = uint64(int64(value__214))
        var t825 uint64 = 0 - t824
        var t826 string = decimal_string(t825)
        var t827 string = "-" + t826
        return t827
    } else {
        var t828 uint64 = uint64(int64(value__214))
        var t829 string = decimal_string(t828)
        return t829
    }
}

func decimal_string(value__208 uint64) string {
    var t852 bool = value__208 == 0
    if t852 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop845:
        for {
            var t846 bool = remaining__210 > 0
            if t846 {
                var t847_rhs uint64 = 10
                var t847 uint64 = remaining__210 % t847_rhs
                var t848 uint8 = uint8(uint64(t847))
                var t849 uint8 = t848 + 48
                vec_push__Vec_5uint8(reversed__209, t849)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t850 uint64 = compound_old353 / compound_value354
                remaining__210 = t850
                continue
            } else {
                break Loop_loop845
            }
        }
        var t834 int
        var inline883 int = vec_len__Vec_5uint8(reversed__209)
        t834 = inline883
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t834)
        var offset__212 int = 0
        Loop_loop836:
        for {
            var t837 int
            var inline881 int = vec_len__Vec_5uint8(reversed__209)
            t837 = inline881
            var t838 bool = offset__212 < t837
            if t838 {
                var t839 int
                var inline879 int = vec_len__Vec_5uint8(reversed__209)
                t839 = inline879
                var t840 int = t839 - offset__212
                var t841 int = t840 - 1
                var t842 uint8 = vec_get__Vec_5uint8(reversed__209, t841)
                vec_push__Vec_5uint8(bytes__211, t842)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t843 int = compound_old358 + compound_value359
                offset__212 = t843
                continue
            } else {
                break Loop_loop836
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
