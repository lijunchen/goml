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
    var inline904 int = 0
    var inline905 *ref_int_x = ref__Ref_3int(inline904)
    i__0 = inline905
    Loop_loop804:
    for {
        var t805 int
        var inline898 int = ref_get__Ref_3int(i__0)
        t805 = inline898
        var t806 bool = t805 < 8
        if t806 {
            var t807 int
            var inline896 int = ref_get__Ref_3int(i__0)
            t807 = inline896
            var t808 int = t807 + 1
            ref_set__Ref_3int(i__0, t808)
            var t814 int
            var inline892 int = ref_get__Ref_3int(i__0)
            t814 = inline892
            var t815 bool = t814 == 3
            if t815 {
                continue
            } else {
                var t812 int
                var inline890 int = ref_get__Ref_3int(i__0)
                t812 = inline890
                var t813 bool = t812 == 6
                if t813 {
                    continue
                } else {
                    var t811 int
                    var inline888 int = ref_get__Ref_3int(i__0)
                    t811 = inline888
                    var inline885 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t811)
                    _goml_runtime_core_string_println(inline885)
                    continue
                }
            }
        } else {
            break Loop_loop804
        }
    }
    var inline900 string = "done"
    var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline900)
    _goml_runtime_core_string_println(inline901)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline910 int64 = int64(int(self__404))
    var inline911 string = signed_decimal_string(inline910)
    return inline911
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t843 bool = value__214 < 0
    if t843 {
        var t844 uint64 = uint64(int64(value__214))
        var t845 uint64 = 0 - t844
        var t846 string = decimal_string(t845)
        var t847 string = "-" + t846
        return t847
    } else {
        var t848 uint64 = uint64(int64(value__214))
        var t849 string = decimal_string(t848)
        return t849
    }
}

func decimal_string(value__208 uint64) string {
    var t872 bool = value__208 == 0
    if t872 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop865:
        for {
            var t866 bool = remaining__210 > 0
            if t866 {
                var t867_rhs uint64 = 10
                var t867 uint64 = remaining__210 % t867_rhs
                var t868 uint8 = uint8(uint64(t867))
                var t869 uint8 = t868 + 48
                vec_push__Vec_5uint8(reversed__209, t869)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t870 uint64 = compound_old353 / compound_value354
                remaining__210 = t870
                continue
            } else {
                break Loop_loop865
            }
        }
        var t854 int
        var inline929 int = vec_len__Vec_5uint8(reversed__209)
        t854 = inline929
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t854)
        var offset__212 int = 0
        Loop_loop856:
        for {
            var t857 int
            var inline927 int = vec_len__Vec_5uint8(reversed__209)
            t857 = inline927
            var t858 bool = offset__212 < t857
            if t858 {
                var t859 int
                var inline925 int = vec_len__Vec_5uint8(reversed__209)
                t859 = inline925
                var t860 int = t859 - offset__212
                var t861 int = t860 - 1
                var t862 uint8 = vec_get__Vec_5uint8(reversed__209, t861)
                vec_push__Vec_5uint8(bytes__211, t862)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t863 int = compound_old358 + compound_value359
                offset__212 = t863
                continue
            } else {
                break Loop_loop856
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
