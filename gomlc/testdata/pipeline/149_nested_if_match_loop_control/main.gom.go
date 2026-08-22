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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
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
    var i__0 *ref_int_x
    var inline896 int = 0
    var inline897 *ref_int_x = ref__Ref_3int(inline896)
    i__0 = inline897
    var sum__1 *ref_int_x
    var inline893 int = 0
    var inline894 *ref_int_x = ref__Ref_3int(inline893)
    sum__1 = inline894
    Loop_loop804:
    for {
        var t805 int
        var inline886 int = ref_get__Ref_3int(i__0)
        t805 = inline886
        var t806 bool = t805 < 7
        if t806 {
            var cur__2 int
            var inline884 int = ref_get__Ref_3int(i__0)
            cur__2 = inline884
            var t807 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t807)
            var t811 bool = cur__2 < 5
            if t811 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t809 int
                    var inline880 int = ref_get__Ref_3int(sum__1)
                    t809 = inline880
                    var t810 int = t809 + cur__2
                    ref_set__Ref_3int(sum__1, t810)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop804
                default:
                    var t809 int
                    var inline880 int = ref_get__Ref_3int(sum__1)
                    t809 = inline880
                    var t810 int = t809 + cur__2
                    ref_set__Ref_3int(sum__1, t810)
                    continue
                }
            }
        } else {
            break Loop_loop804
        }
    }
    var t803 int
    var inline891 int = ref_get__Ref_3int(sum__1)
    t803 = inline891
    var inline888 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t803)
    _goml_runtime_core_string_println(inline888)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline901 int64 = int64(int(self__404))
    var inline902 string = signed_decimal_string(inline901)
    return inline902
}

func signed_decimal_string(value__214 int64) string {
    var t836 bool = value__214 < 0
    if t836 {
        var t837 uint64 = uint64(int64(value__214))
        var t838 uint64 = 0 - t837
        var t839 string = decimal_string(t838)
        var t840 string = "-" + t839
        return t840
    } else {
        var t841 uint64 = uint64(int64(value__214))
        var t842 string = decimal_string(t841)
        return t842
    }
}

func decimal_string(value__208 uint64) string {
    var t865 bool = value__208 == 0
    if t865 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop858:
        for {
            var t859 bool = remaining__210 > 0
            if t859 {
                var t860_rhs uint64 = 10
                var t860 uint64 = remaining__210 % t860_rhs
                var t861 uint8 = uint8(uint64(t860))
                var t862 uint8 = t861 + 48
                vec_push__Vec_5uint8(reversed__209, t862)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t863 uint64 = compound_old353 / compound_value354
                remaining__210 = t863
                continue
            } else {
                break Loop_loop858
            }
        }
        var t847 int
        var inline920 int = vec_len__Vec_5uint8(reversed__209)
        t847 = inline920
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t847)
        var offset__212 int = 0
        Loop_loop849:
        for {
            var t850 int
            var inline918 int = vec_len__Vec_5uint8(reversed__209)
            t850 = inline918
            var t851 bool = offset__212 < t850
            if t851 {
                var t852 int
                var inline916 int = vec_len__Vec_5uint8(reversed__209)
                t852 = inline916
                var t853 int = t852 - offset__212
                var t854 int = t853 - 1
                var t855 uint8 = vec_get__Vec_5uint8(reversed__209, t854)
                vec_push__Vec_5uint8(bytes__211, t855)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t856 int = compound_old358 + compound_value359
                offset__212 = t856
                continue
            } else {
                break Loop_loop849
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
