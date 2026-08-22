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
    var sum__0 *ref_int_x
    var inline923 int = 0
    var inline924 *ref_int_x = ref__Ref_3int(inline923)
    sum__0 = inline924
    var i__1 *ref_int_x
    var inline920 int = 0
    var inline921 *ref_int_x = ref__Ref_3int(inline920)
    i__1 = inline921
    Loop_loop808:
    for {
        var t809 int
        var inline900 int = ref_get__Ref_3int(i__1)
        t809 = inline900
        var t810 bool = t809 < 20
        if t810 {
            var t811 int
            var inline898 int = ref_get__Ref_3int(i__1)
            t811 = inline898
            var t812 int = t811 + 1
            ref_set__Ref_3int(i__1, t812)
            var t817 int
            var inline894 int = ref_get__Ref_3int(i__1)
            t817 = inline894
            var t818 bool = t817 > 5
            if t818 {
                break Loop_loop808
            } else {
                var t814 int
                var inline892 int = ref_get__Ref_3int(sum__0)
                t814 = inline892
                var t815 int
                var inline890 int = ref_get__Ref_3int(i__1)
                t815 = inline890
                var t816 int = t814 + t815
                ref_set__Ref_3int(sum__0, t816)
                continue
            }
        } else {
            break Loop_loop808
        }
    }
    var inline916 string = "sum: "
    var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline916)
    _goml_runtime_core_string_print(inline917)
    var t806 int
    var inline914 int = ref_get__Ref_3int(sum__0)
    t806 = inline914
    var inline911 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t806)
    _goml_runtime_core_string_println(inline911)
    var inline907 string = "i at break: "
    var inline908 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline907)
    _goml_runtime_core_string_print(inline908)
    var t807 int
    var inline905 int = ref_get__Ref_3int(i__1)
    t807 = inline905
    var inline902 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t807)
    _goml_runtime_core_string_println(inline902)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline929 int64 = int64(int(self__404))
    var inline930 string = signed_decimal_string(inline929)
    return inline930
}

func signed_decimal_string(value__214 int64) string {
    var t846 bool = value__214 < 0
    if t846 {
        var t847 uint64 = uint64(int64(value__214))
        var t848 uint64 = 0 - t847
        var t849 string = decimal_string(t848)
        var t850 string = "-" + t849
        return t850
    } else {
        var t851 uint64 = uint64(int64(value__214))
        var t852 string = decimal_string(t851)
        return t852
    }
}

func decimal_string(value__208 uint64) string {
    var t875 bool = value__208 == 0
    if t875 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop868:
        for {
            var t869 bool = remaining__210 > 0
            if t869 {
                var t870_rhs uint64 = 10
                var t870 uint64 = remaining__210 % t870_rhs
                var t871 uint8 = uint8(uint64(t870))
                var t872 uint8 = t871 + 48
                vec_push__Vec_5uint8(reversed__209, t872)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t873 uint64 = compound_old353 / compound_value354
                remaining__210 = t873
                continue
            } else {
                break Loop_loop868
            }
        }
        var t857 int
        var inline948 int = vec_len__Vec_5uint8(reversed__209)
        t857 = inline948
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t857)
        var offset__212 int = 0
        Loop_loop859:
        for {
            var t860 int
            var inline946 int = vec_len__Vec_5uint8(reversed__209)
            t860 = inline946
            var t861 bool = offset__212 < t860
            if t861 {
                var t862 int
                var inline944 int = vec_len__Vec_5uint8(reversed__209)
                t862 = inline944
                var t863 int = t862 - offset__212
                var t864 int = t863 - 1
                var t865 uint8 = vec_get__Vec_5uint8(reversed__209, t864)
                vec_push__Vec_5uint8(bytes__211, t865)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t866 int = compound_old358 + compound_value359
                offset__212 = t866
                continue
            } else {
                break Loop_loop859
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
