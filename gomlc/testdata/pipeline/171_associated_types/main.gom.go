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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

type Ordering int32

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var t801 int32 = self__0.value
    return t801
}

func main0() struct{} {
    var t803 Number = Number{
        value: 42,
    }
    var t804 int32
    var inline902 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t803)
    t804 = inline902
    var t805 string
    var inline900 string = __goml_builtin_int32_to_string(t804)
    t805 = inline900
    var inline897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline897)
    var t806 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline895 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t806)
    value__4 = inline895
    var t807 string
    var inline893 string = __goml_builtin_int32_to_string(value__4)
    t807 = inline893
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline890)
    var t808 Box__string = Box__string{
        value: "generic",
    }
    var t809 string
    var inline888 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t808)
    t809 = inline888
    var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline885)
    var t811 int32
    var inline883 int32 = 11
    t811 = inline883
    var t812 string
    var inline881 string = __goml_builtin_int32_to_string(t811)
    t812 = inline881
    var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline878)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t833 int64 = int64(int32(value__225))
    var inline914 bool = t833 < 0
    if inline914 {
        var inline915 uint64 = uint64(int64(t833))
        var inline916 uint64 = 0 - inline915
        var inline917 string = decimal_string(inline916)
        var inline918 string = "-" + inline917
        return inline918
    } else {
        var inline919 uint64 = uint64(int64(t833))
        var inline920 string = decimal_string(inline919)
        return inline920
    }
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t837 string = self__1.value
    return t837
}

func decimal_string(value__208 uint64) string {
    var t871 bool = value__208 == 0
    if t871 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop864:
        for {
            var t865 bool = remaining__210 > 0
            if t865 {
                var t866_rhs uint64 = 10
                var t866 uint64 = remaining__210 % t866_rhs
                var t867 uint8 = uint8(uint64(t866))
                var t868 uint8 = t867 + 48
                vec_push__Vec_5uint8(reversed__209, t868)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t869 uint64 = compound_old353 / compound_value354
                remaining__210 = t869
                continue
            } else {
                break Loop_loop864
            }
        }
        var t853 int
        var inline930 int = vec_len__Vec_5uint8(reversed__209)
        t853 = inline930
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t853)
        var offset__212 int = 0
        Loop_loop855:
        for {
            var t856 int
            var inline928 int = vec_len__Vec_5uint8(reversed__209)
            t856 = inline928
            var t857 bool = offset__212 < t856
            if t857 {
                var t858 int
                var inline926 int = vec_len__Vec_5uint8(reversed__209)
                t858 = inline926
                var t859 int = t858 - offset__212
                var t860 int = t859 - 1
                var t861 uint8 = vec_get__Vec_5uint8(reversed__209, t860)
                vec_push__Vec_5uint8(bytes__211, t861)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t862 int = compound_old358 + compound_value359
                offset__212 = t862
                continue
            } else {
                break Loop_loop855
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
