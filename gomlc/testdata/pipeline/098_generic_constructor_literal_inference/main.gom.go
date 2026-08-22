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

type Option__u8 struct {
    _tag int32
    _v0_0 uint8
}

func main0() struct{} {
    var x796 uint8 = 42
    var t800 string
    var inline847 string = __goml_builtin_uint8_to_string(x796)
    t800 = inline847
    var inline844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline844)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t813 uint64 = uint64(uint8(value__228))
    var t814 string = decimal_string(t813)
    return t814
}

func decimal_string(value__208 uint64) string {
    var t837 bool = value__208 == 0
    if t837 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop830:
        for {
            var t831 bool = remaining__210 > 0
            if t831 {
                var t832_rhs uint64 = 10
                var t832 uint64 = remaining__210 % t832_rhs
                var t833 uint8 = uint8(uint64(t832))
                var t834 uint8 = t833 + 48
                vec_push__Vec_5uint8(reversed__209, t834)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t835 uint64 = compound_old353 / compound_value354
                remaining__210 = t835
                continue
            } else {
                break Loop_loop830
            }
        }
        var t819 int
        var inline861 int = vec_len__Vec_5uint8(reversed__209)
        t819 = inline861
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t819)
        var offset__212 int = 0
        Loop_loop821:
        for {
            var t822 int
            var inline859 int = vec_len__Vec_5uint8(reversed__209)
            t822 = inline859
            var t823 bool = offset__212 < t822
            if t823 {
                var t824 int
                var inline857 int = vec_len__Vec_5uint8(reversed__209)
                t824 = inline857
                var t825 int = t824 - offset__212
                var t826 int = t825 - 1
                var t827 uint8 = vec_get__Vec_5uint8(reversed__209, t826)
                vec_push__Vec_5uint8(bytes__211, t827)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t828 int = compound_old358 + compound_value359
                offset__212 = t828
                continue
            } else {
                break Loop_loop821
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
