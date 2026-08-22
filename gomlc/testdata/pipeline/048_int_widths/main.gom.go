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
    var start16__0 int16 = 300
    var delta16__1 int16 = 45
    var sum16__2 int16 = start16__0 + delta16__1
    var flipped16__3 int16 = -start16__0
    var base32__4 int32 = 100000
    var more32__5 int32 = 200000
    var sum32__6 int32 = base32__4 + more32__5
    var diff32__7 int32 = sum32__6 - base32__4
    var big64__8 int64 = 5000000000
    var step64__9 int64 = 2000000000
    var remain64__10 int64 = big64__8 - step64__9
    var neg64__11 int64 = -step64__9
    var t798 string
    var inline886 string = __goml_builtin_int16_to_string(sum16__2)
    t798 = inline886
    var t799 string = t798 + ", "
    var t800 string
    var inline884 string = __goml_builtin_int16_to_string(flipped16__3)
    t800 = inline884
    var t801 string = t799 + t800
    var t802 string = t801 + "; "
    var t803 string
    var inline882 string = __goml_builtin_int32_to_string(diff32__7)
    t803 = inline882
    var t804 string = t802 + t803
    var t805 string = t804 + "; "
    var t806 string
    var inline880 string = __goml_builtin_int64_to_string(remain64__10)
    t806 = inline880
    var t807 string = t805 + t806
    var t808 string = t807 + "; "
    var t809 string
    var inline878 string = __goml_builtin_int64_to_string(neg64__11)
    t809 = inline878
    var message__12 string = t808 + t809
    var inline875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline875)
    return struct{}{}
}

func __goml_builtin_int16_to_string(value__224 int16) string {
    var t824 int64 = int64(int16(value__224))
    var inline897 bool = t824 < 0
    if inline897 {
        var inline898 uint64 = uint64(int64(t824))
        var inline899 uint64 = 0 - inline898
        var inline900 string = decimal_string(inline899)
        var inline901 string = "-" + inline900
        return inline901
    } else {
        var inline902 uint64 = uint64(int64(t824))
        var inline903 string = decimal_string(inline902)
        return inline903
    }
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t828 int64 = int64(int32(value__225))
    var inline905 bool = t828 < 0
    if inline905 {
        var inline906 uint64 = uint64(int64(t828))
        var inline907 uint64 = 0 - inline906
        var inline908 string = decimal_string(inline907)
        var inline909 string = "-" + inline908
        return inline909
    } else {
        var inline910 uint64 = uint64(int64(t828))
        var inline911 string = decimal_string(inline910)
        return inline911
    }
}

func __goml_builtin_int64_to_string(value__226 int64) string {
    var inline913 bool = value__226 < 0
    if inline913 {
        var inline914 uint64 = uint64(int64(value__226))
        var inline915 uint64 = 0 - inline914
        var inline916 string = decimal_string(inline915)
        var inline917 string = "-" + inline916
        return inline917
    } else {
        var inline918 uint64 = uint64(int64(value__226))
        var inline919 string = decimal_string(inline918)
        return inline919
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t868 bool = value__208 == 0
    if t868 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop861:
        for {
            var t862 bool = remaining__210 > 0
            if t862 {
                var t863_rhs uint64 = 10
                var t863 uint64 = remaining__210 % t863_rhs
                var t864 uint8 = uint8(uint64(t863))
                var t865 uint8 = t864 + 48
                vec_push__Vec_5uint8(reversed__209, t865)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t866 uint64 = compound_old353 / compound_value354
                remaining__210 = t866
                continue
            } else {
                break Loop_loop861
            }
        }
        var t850 int
        var inline929 int = vec_len__Vec_5uint8(reversed__209)
        t850 = inline929
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t850)
        var offset__212 int = 0
        Loop_loop852:
        for {
            var t853 int
            var inline927 int = vec_len__Vec_5uint8(reversed__209)
            t853 = inline927
            var t854 bool = offset__212 < t853
            if t854 {
                var t855 int
                var inline925 int = vec_len__Vec_5uint8(reversed__209)
                t855 = inline925
                var t856 int = t855 - offset__212
                var t857 int = t856 - 1
                var t858 uint8 = vec_get__Vec_5uint8(reversed__209, t857)
                vec_push__Vec_5uint8(bytes__211, t858)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t859 int = compound_old358 + compound_value359
                offset__212 = t859
                continue
            } else {
                break Loop_loop852
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
