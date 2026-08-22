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

type Result__i32__string struct {
    _tag int32
    _v0_0 string
    _v1_0 int32
}

func parse(flag__0 bool) Result__i32__string {
    if flag__0 {
        var t806 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: 41,
        }
        return t806
    } else {
        var t807 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        return t807
    }
}

func compute(flag__1 bool) Result__i32__string {
    var mtmp796 Result__i32__string
    if flag__1 {
        var inline878 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: 41,
        }
        mtmp796 = inline878
    } else {
        var inline879 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        mtmp796 = inline879
    }
    var jp811 int32
    switch mtmp796._tag {
    case 0:
        var x797 string = mtmp796._v0_0
        var t814 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: x797,
        }
        return t814
    case 1:
        var x798 int32 = mtmp796._v1_0
        jp811 = x798
        var t812 int32 = jp811 + 1
        var t813 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: t812,
        }
        return t813
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t821 Result__i32__string = compute(true)
    var t822 string
    switch t821._tag {
    case 0:
        var inline908 string = t821._v0_0
        t822 = inline908
    case 1:
        var inline910 int32 = t821._v1_0
        var inline912 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline910)
        t822 = inline912
    default:
        panic("non-exhaustive match")
    }
    var inline905 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline905)
    var t823 Result__i32__string
    var inline892 bool = false
    var inline893 Result__i32__string = parse(inline892)
    var inline895 int32
    switch inline893._tag {
    case 0:
        var inline899 string = inline893._v0_0
        var inline901 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: inline899,
        }
        t823 = inline901
        var t824 string
        switch t823._tag {
        case 0:
            var inline886 string = t823._v0_0
            t824 = inline886
        case 1:
            var inline888 int32 = t823._v1_0
            var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
            t824 = inline890
        default:
            panic("non-exhaustive match")
        }
        var inline883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
        _goml_runtime_core_string_println(inline883)
        return struct{}{}
    case 1:
        var inline902 int32 = inline893._v1_0
        inline895 = inline902
        var inline897 int32 = inline895 + 1
        var inline898 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: inline897,
        }
        t823 = inline898
        var t824 string
        switch t823._tag {
        case 0:
            var inline886 string = t823._v0_0
            t824 = inline886
        case 1:
            var inline888 int32 = t823._v1_0
            var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
            t824 = inline890
        default:
            panic("non-exhaustive match")
        }
        var inline883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
        _goml_runtime_core_string_println(inline883)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline914 int64 = int64(int32(self__286))
    var inline915 string = signed_decimal_string(inline914)
    return inline915
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t842 bool = value__214 < 0
    if t842 {
        var t843 uint64 = uint64(int64(value__214))
        var t844 uint64 = 0 - t843
        var t845 string = decimal_string(t844)
        var t846 string = "-" + t845
        return t846
    } else {
        var t847 uint64 = uint64(int64(value__214))
        var t848 string = decimal_string(t847)
        return t848
    }
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
        var inline934 int = vec_len__Vec_5uint8(reversed__209)
        t853 = inline934
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t853)
        var offset__212 int = 0
        Loop_loop855:
        for {
            var t856 int
            var inline932 int = vec_len__Vec_5uint8(reversed__209)
            t856 = inline932
            var t857 bool = offset__212 < t856
            if t857 {
                var t858 int
                var inline930 int = vec_len__Vec_5uint8(reversed__209)
                t858 = inline930
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
