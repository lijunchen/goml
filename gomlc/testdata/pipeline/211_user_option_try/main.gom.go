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
    _v0_0 int32
}

func maybe_value(flag__0 bool) Option__i32 {
    if flag__0 {
        var t804 Option__i32 = Option__i32{
            _tag: 0,
            _v0_0: 41,
        }
        return t804
    } else {
        return Option__i32{
            _tag: 1,
        }
    }
}

func main0() struct{} {
    var t817 Option__i32
    var inline902 bool = true
    var inline903 Option__i32 = maybe_value(inline902)
    var inline905 int32
    switch inline903._tag {
    case 0:
        var inline909 int32 = inline903._v0_0
        inline905 = inline909
        var inline907 int32 = inline905 + 1
        var inline908 Option__i32 = Option__i32{
            _tag: 0,
            _v0_0: inline907,
        }
        t817 = inline908
        var t818 string
        switch t817._tag {
        case 0:
            var inline898 int32 = t817._v0_0
            var inline900 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline898)
            t818 = inline900
        case 1:
            t818 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
        _goml_runtime_core_string_println(inline895)
        var t819 Option__i32
        var inline885 bool = false
        var inline886 Option__i32 = maybe_value(inline885)
        var inline888 int32
        switch inline886._tag {
        case 0:
            var inline892 int32 = inline886._v0_0
            inline888 = inline892
            var inline890 int32 = inline888 + 1
            var inline891 Option__i32 = Option__i32{
                _tag: 0,
                _v0_0: inline890,
            }
            t819 = inline891
            var t820 string
            switch t819._tag {
            case 0:
                var inline881 int32 = t819._v0_0
                var inline883 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline881)
                t820 = inline883
            case 1:
                t820 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline878)
            return struct{}{}
        case 1:
            t819 = Option__i32{
                _tag: 1,
            }
            var t820 string
            switch t819._tag {
            case 0:
                var inline881 int32 = t819._v0_0
                var inline883 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline881)
                t820 = inline883
            case 1:
                t820 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline878)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        t817 = Option__i32{
            _tag: 1,
        }
        var t818 string
        switch t817._tag {
        case 0:
            var inline898 int32 = t817._v0_0
            var inline900 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline898)
            t818 = inline900
        case 1:
            t818 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
        _goml_runtime_core_string_println(inline895)
        var t819 Option__i32
        var inline885 bool = false
        var inline886 Option__i32 = maybe_value(inline885)
        var inline888 int32
        switch inline886._tag {
        case 0:
            var inline892 int32 = inline886._v0_0
            inline888 = inline892
            var inline890 int32 = inline888 + 1
            var inline891 Option__i32 = Option__i32{
                _tag: 0,
                _v0_0: inline890,
            }
            t819 = inline891
            var t820 string
            switch t819._tag {
            case 0:
                var inline881 int32 = t819._v0_0
                var inline883 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline881)
                t820 = inline883
            case 1:
                t820 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline878)
            return struct{}{}
        case 1:
            t819 = Option__i32{
                _tag: 1,
            }
            var t820 string
            switch t819._tag {
            case 0:
                var inline881 int32 = t819._v0_0
                var inline883 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline881)
                t820 = inline883
            case 1:
                t820 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline878)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline912 int64 = int64(int32(self__286))
    var inline913 string = signed_decimal_string(inline912)
    return inline913
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t838 bool = value__214 < 0
    if t838 {
        var t839 uint64 = uint64(int64(value__214))
        var t840 uint64 = 0 - t839
        var t841 string = decimal_string(t840)
        var t842 string = "-" + t841
        return t842
    } else {
        var t843 uint64 = uint64(int64(value__214))
        var t844 string = decimal_string(t843)
        return t844
    }
}

func decimal_string(value__208 uint64) string {
    var t867 bool = value__208 == 0
    if t867 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop860:
        for {
            var t861 bool = remaining__210 > 0
            if t861 {
                var t862_rhs uint64 = 10
                var t862 uint64 = remaining__210 % t862_rhs
                var t863 uint8 = uint8(uint64(t862))
                var t864 uint8 = t863 + 48
                vec_push__Vec_5uint8(reversed__209, t864)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t865 uint64 = compound_old353 / compound_value354
                remaining__210 = t865
                continue
            } else {
                break Loop_loop860
            }
        }
        var t849 int
        var inline932 int = vec_len__Vec_5uint8(reversed__209)
        t849 = inline932
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t849)
        var offset__212 int = 0
        Loop_loop851:
        for {
            var t852 int
            var inline930 int = vec_len__Vec_5uint8(reversed__209)
            t852 = inline930
            var t853 bool = offset__212 < t852
            if t853 {
                var t854 int
                var inline928 int = vec_len__Vec_5uint8(reversed__209)
                t854 = inline928
                var t855 int = t854 - offset__212
                var t856 int = t855 - 1
                var t857 uint8 = vec_get__Vec_5uint8(reversed__209, t856)
                vec_push__Vec_5uint8(bytes__211, t857)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t858 int = compound_old358 + compound_value359
                offset__212 = t858
                continue
            } else {
                break Loop_loop851
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
