package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    var inline935 string = "sum="
    var inline936 string = _goml_m_inherent_i_i32_i_i32_i_to__string(sum__5)
    var inline937 string = inline935 + inline936
    println__T_string(inline937)
    var inline930 string = "diff="
    var inline931 string = _goml_m_inherent_i_i32_i_i32_i_to__string(diff__6)
    var inline932 string = inline930 + inline931
    println__T_string(inline932)
    var inline925 string = "prod="
    var inline926 string = _goml_m_inherent_i_i32_i_i32_i_to__string(prod__7)
    var inline927 string = inline925 + inline926
    println__T_string(inline927)
    var inline920 string = "quot="
    var inline921 string = _goml_m_inherent_i_i32_i_i32_i_to__string(quot__8)
    var inline922 string = inline920 + inline921
    println__T_string(inline922)
    var jp816 bool
    jp816 = false
    var jp818 bool
    jp818 = true
    var not_result__11 bool = !false
    var t829 bool = !jp816
    var jp822 bool
    if t829 {
        var t830 int32 = prod__7 * base__4
        var t831 int32 = sum__5 + t830
        var t832 int32 = prod__7 / 2
        var mtmp802 int32 = t831 - t832
        switch mtmp802 {
        case 0:
            jp822 = false
        default:
            jp822 = true
        }
    } else {
        jp822 = false
    }
    var jp820 bool
    if jp822 {
        jp820 = true
    } else {
        var t823 int32 = diff__6 - quot__8
        var t824 int32 = t823 + base__4
        var t825 int32 = sum__5 / 2
        var mtmp803 int32 = t824 - t825
        var jp827 bool
        switch mtmp803 {
        case 0:
            jp827 = false
        default:
            jp827 = true
        }
        var t828 bool = !jp827
        jp820 = t828
    }
    var inline915 string = "and="
    var inline916 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp816)
    var inline917 string = inline915 + inline916
    println__T_string(inline917)
    var inline910 string = "or="
    var inline911 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp818)
    var inline912 string = inline910 + inline911
    println__T_string(inline912)
    var inline905 string = "not="
    var inline906 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline907 string = inline905 + inline906
    println__T_string(inline907)
    var inline900 string = "mixed="
    var inline901 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp820)
    var inline902 string = inline900 + inline901
    println__T_string(inline902)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t836 string
    t836 = value__1
    _goml_runtime_core_string_println(t836)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline941 int64 = int64(int32(self__286))
    var inline942 string = signed_decimal_string(inline941)
    return inline942
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t843 string = _goml_runtime_core_bool_to_string(self__401)
    return t843
}

func signed_decimal_string(value__214 int64) string {
    var t854 bool = value__214 < 0
    if t854 {
        var t855 uint64 = uint64(int64(value__214))
        var t856 uint64 = 0 - t855
        var t857 string = decimal_string(t856)
        var t858 string = "-" + t857
        return t858
    } else {
        var t859 uint64 = uint64(int64(value__214))
        var t860 string = decimal_string(t859)
        return t860
    }
}

func decimal_string(value__208 uint64) string {
    var t883 bool = value__208 == 0
    if t883 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop876:
        for {
            var t877 bool = remaining__210 > 0
            if t877 {
                var t878_rhs uint64 = 10
                var t878 uint64 = remaining__210 % t878_rhs
                var t879 uint8 = uint8(uint64(t878))
                var t880 uint8 = t879 + 48
                vec_push__Vec_5uint8(reversed__209, t880)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t881 uint64 = compound_old353 / compound_value354
                remaining__210 = t881
                continue
            } else {
                break Loop_loop876
            }
        }
        var t865 int
        var inline960 int = vec_len__Vec_5uint8(reversed__209)
        t865 = inline960
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t865)
        var offset__212 int = 0
        Loop_loop867:
        for {
            var t868 int
            var inline958 int = vec_len__Vec_5uint8(reversed__209)
            t868 = inline958
            var t869 bool = offset__212 < t868
            if t869 {
                var t870 int
                var inline956 int = vec_len__Vec_5uint8(reversed__209)
                t870 = inline956
                var t871 int = t870 - offset__212
                var t872 int = t871 - 1
                var t873 uint8 = vec_get__Vec_5uint8(reversed__209, t872)
                vec_push__Vec_5uint8(bytes__211, t873)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t874 int = compound_old358 + compound_value359
                offset__212 = t874
                continue
            } else {
                break Loop_loop867
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
