package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

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

type _goml_vec_string struct {
    items []string
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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
    var minimum__0 int64 = -9223372036854775807 - 1
    var maximum__1 int64 = 9223372036854775807
    var unsigned__2 uint64 = 18446744073709551615
    var inline914 string = _goml_m_trait__impl_i_ToString_i_i64_i_to__string(minimum__0)
    _goml_runtime_core_string_println(inline914)
    var inline911 string = _goml_m_trait__impl_i_ToString_i_i64_i_to__string(maximum__1)
    _goml_runtime_core_string_println(inline911)
    var inline908 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(unsigned__2)
    _goml_runtime_core_string_println(inline908)
    var t800 [3]string = [3]string{"go", "ml", "!"}
    var t801 *_goml_vec_string = func(values [3]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [3]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t800)
    var t802 string
    var inline906 string = __goml_builtin_string_concat(t801)
    t802 = inline906
    var inline903 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline903)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i64_i_to__string(self__408 int64) string {
    var inline922 string = signed_decimal_string(self__408)
    return inline922
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__412 uint64) string {
    var inline924 string = decimal_string(self__412)
    return inline924
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_string_concat(values__215 *_goml_vec_string) string {
    var length__216 int = 0
    var value_index__217 int = 0
    Loop_loop840:
    for {
        var t841 int
        var inline928 int = vec_len__Vec_6string(values__215)
        t841 = inline928
        var t842 bool = value_index__217 < t841
        if t842 {
            var compound_old365 int = length__216
            var t843 string = vec_get__Vec_6string(values__215, value_index__217)
            var compound_value366 int
            var inline926 int = _goml_runtime_core_string_len(t843)
            compound_value366 = inline926
            var t844 int = compound_old365 + compound_value366
            length__216 = t844
            var compound_old368 int = value_index__217
            var compound_value369 int = 1
            var t846 int = compound_old368 + compound_value369
            value_index__217 = t846
            continue
        } else {
            break Loop_loop840
        }
    }
    var bytes__218 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(length__216)
    value_index__217 = 0
    Loop_loop828:
    for {
        var t829 int
        var inline936 int = vec_len__Vec_6string(values__215)
        t829 = inline936
        var t830 bool = value_index__217 < t829
        if t830 {
            var value__219 string = vec_get__Vec_6string(values__215, value_index__217)
            var byte_index__220 int = 0
            Loop_loop834:
            for {
                var t835 int
                var inline934 int = _goml_runtime_core_string_len(value__219)
                t835 = inline934
                var t836 bool = byte_index__220 < t835
                if t836 {
                    var t837 uint8
                    var inline932 uint8 = _goml_runtime_core_string_byte_get(value__219, byte_index__220)
                    t837 = inline932
                    vec_push__Vec_5uint8(bytes__218, t837)
                    var compound_old374 int = byte_index__220
                    var compound_value375 int = 1
                    var t838 int = compound_old374 + compound_value375
                    byte_index__220 = t838
                    continue
                } else {
                    break Loop_loop834
                }
            }
            var compound_old378 int = value_index__217
            var compound_value379 int = 1
            var t832 int = compound_old378 + compound_value379
            value_index__217 = t832
            continue
        } else {
            break Loop_loop828
        }
    }
    var mtmp382 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__218)
    var x384 string = mtmp382._1
    return x384
}

func signed_decimal_string(value__214 int64) string {
    var t869 bool = value__214 < 0
    if t869 {
        var t870 uint64 = uint64(int64(value__214))
        var t871 uint64 = 0 - t870
        var t872 string = decimal_string(t871)
        var t873 string = "-" + t872
        return t873
    } else {
        var t874 uint64 = uint64(int64(value__214))
        var t875 string = decimal_string(t874)
        return t875
    }
}

func decimal_string(value__208 uint64) string {
    var t898 bool = value__208 == 0
    if t898 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop891:
        for {
            var t892 bool = remaining__210 > 0
            if t892 {
                var t893_rhs uint64 = 10
                var t893 uint64 = remaining__210 % t893_rhs
                var t894 uint8 = uint8(uint64(t893))
                var t895 uint8 = t894 + 48
                vec_push__Vec_5uint8(reversed__209, t895)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t896 uint64 = compound_old353 / compound_value354
                remaining__210 = t896
                continue
            } else {
                break Loop_loop891
            }
        }
        var t880 int
        var inline954 int = vec_len__Vec_5uint8(reversed__209)
        t880 = inline954
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t880)
        var offset__212 int = 0
        Loop_loop882:
        for {
            var t883 int
            var inline952 int = vec_len__Vec_5uint8(reversed__209)
            t883 = inline952
            var t884 bool = offset__212 < t883
            if t884 {
                var t885 int
                var inline950 int = vec_len__Vec_5uint8(reversed__209)
                t885 = inline950
                var t886 int = t885 - offset__212
                var t887 int = t886 - 1
                var t888 uint8 = vec_get__Vec_5uint8(reversed__209, t887)
                vec_push__Vec_5uint8(bytes__211, t888)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t889 int = compound_old358 + compound_value359
                offset__212 = t889
                continue
            } else {
                break Loop_loop882
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
