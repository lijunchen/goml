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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func maybe_value(flag__0 bool) Option__i32 {
    if flag__0 {
        var t805 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 4,
        }
        return t805
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t808 int32 = a__1 + b__2
    return t808
}

func main0() struct{} {
    var t822 Option__i32
    var inline910 bool = true
    var inline911 Option__i32 = maybe_value(inline910)
    var inline913 int32
    switch inline911._tag {
    case 0:
        t822 = Option__i32{
            _tag: 0,
        }
        var t823 string
        switch t822._tag {
        case 0:
            t823 = "none"
        case 1:
            var inline905 int32 = t822._v1_0
            var inline907 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline905)
            var inline908 string = "some=" + inline907
            t823 = inline908
        default:
            panic("non-exhaustive match")
        }
        var inline902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
        _goml_runtime_core_string_println(inline902)
        var t824 Option__i32
        var inline893 bool = false
        var inline894 Option__i32 = maybe_value(inline893)
        var inline896 int32
        switch inline894._tag {
        case 0:
            t824 = Option__i32{
                _tag: 0,
            }
            var t825 string
            switch t824._tag {
            case 0:
                t825 = "none"
            case 1:
                var inline888 int32 = t824._v1_0
                var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
                var inline891 string = "some=" + inline890
                t825 = inline891
            default:
                panic("non-exhaustive match")
            }
            var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
            _goml_runtime_core_string_println(inline885)
            return struct{}{}
        case 1:
            var inline899 int32 = inline894._v1_0
            inline896 = inline899
            var inline897 int32 = add(inline896, 2)
            var inline898 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline897,
            }
            t824 = inline898
            var t825 string
            switch t824._tag {
            case 0:
                t825 = "none"
            case 1:
                var inline888 int32 = t824._v1_0
                var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
                var inline891 string = "some=" + inline890
                t825 = inline891
            default:
                panic("non-exhaustive match")
            }
            var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
            _goml_runtime_core_string_println(inline885)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline916 int32 = inline911._v1_0
        inline913 = inline916
        var inline914 int32 = add(inline913, 2)
        var inline915 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: inline914,
        }
        t822 = inline915
        var t823 string
        switch t822._tag {
        case 0:
            t823 = "none"
        case 1:
            var inline905 int32 = t822._v1_0
            var inline907 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline905)
            var inline908 string = "some=" + inline907
            t823 = inline908
        default:
            panic("non-exhaustive match")
        }
        var inline902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
        _goml_runtime_core_string_println(inline902)
        var t824 Option__i32
        var inline893 bool = false
        var inline894 Option__i32 = maybe_value(inline893)
        var inline896 int32
        switch inline894._tag {
        case 0:
            t824 = Option__i32{
                _tag: 0,
            }
            var t825 string
            switch t824._tag {
            case 0:
                t825 = "none"
            case 1:
                var inline888 int32 = t824._v1_0
                var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
                var inline891 string = "some=" + inline890
                t825 = inline891
            default:
                panic("non-exhaustive match")
            }
            var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
            _goml_runtime_core_string_println(inline885)
            return struct{}{}
        case 1:
            var inline899 int32 = inline894._v1_0
            inline896 = inline899
            var inline897 int32 = add(inline896, 2)
            var inline898 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline897,
            }
            t824 = inline898
            var t825 string
            switch t824._tag {
            case 0:
                t825 = "none"
            case 1:
                var inline888 int32 = t824._v1_0
                var inline890 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline888)
                var inline891 string = "some=" + inline890
                t825 = inline891
            default:
                panic("non-exhaustive match")
            }
            var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
            _goml_runtime_core_string_println(inline885)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline919 int64 = int64(int32(self__286))
    var inline920 string = signed_decimal_string(inline919)
    return inline920
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
        var inline939 int = vec_len__Vec_5uint8(reversed__209)
        t853 = inline939
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t853)
        var offset__212 int = 0
        Loop_loop855:
        for {
            var t856 int
            var inline937 int = vec_len__Vec_5uint8(reversed__209)
            t856 = inline937
            var t857 bool = offset__212 < t856
            if t857 {
                var t858 int
                var inline935 int = vec_len__Vec_5uint8(reversed__209)
                t858 = inline935
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
