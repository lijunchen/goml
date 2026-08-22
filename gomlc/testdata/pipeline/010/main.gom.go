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
    var x796 bool = true
    var x797 bool = true
    switch x797 {
    case true:
        switch x796 {
        case true:
            var t801 string
            var inline865 int = 789
            var inline866 string = __goml_builtin_int_to_string(inline865)
            t801 = inline866
            var inline862 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
            _goml_runtime_core_string_println(inline862)
            return struct{}{}
        case false:
            var t803 string
            var inline871 int = 456
            var inline872 string = __goml_builtin_int_to_string(inline871)
            t803 = inline872
            var inline868 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
            _goml_runtime_core_string_println(inline868)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x796 {
        case true:
            var t806 string
            var inline877 int = 123
            var inline878 string = __goml_builtin_int_to_string(inline877)
            t806 = inline878
            var inline874 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
            _goml_runtime_core_string_println(inline874)
            return struct{}{}
        case false:
            var t808 string
            var inline883 int = 789
            var inline884 string = __goml_builtin_int_to_string(inline883)
            t808 = inline884
            var inline880 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
            _goml_runtime_core_string_println(inline880)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t820 int64 = int64(int(value__222))
    var inline890 bool = t820 < 0
    if inline890 {
        var inline891 uint64 = uint64(int64(t820))
        var inline892 uint64 = 0 - inline891
        var inline893 string = decimal_string(inline892)
        var inline894 string = "-" + inline893
        return inline894
    } else {
        var inline895 uint64 = uint64(int64(t820))
        var inline896 string = decimal_string(inline895)
        return inline896
    }
}

func decimal_string(value__208 uint64) string {
    var t855 bool = value__208 == 0
    if t855 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop848:
        for {
            var t849 bool = remaining__210 > 0
            if t849 {
                var t850_rhs uint64 = 10
                var t850 uint64 = remaining__210 % t850_rhs
                var t851 uint8 = uint8(uint64(t850))
                var t852 uint8 = t851 + 48
                vec_push__Vec_5uint8(reversed__209, t852)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t853 uint64 = compound_old353 / compound_value354
                remaining__210 = t853
                continue
            } else {
                break Loop_loop848
            }
        }
        var t837 int
        var inline906 int = vec_len__Vec_5uint8(reversed__209)
        t837 = inline906
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t837)
        var offset__212 int = 0
        Loop_loop839:
        for {
            var t840 int
            var inline904 int = vec_len__Vec_5uint8(reversed__209)
            t840 = inline904
            var t841 bool = offset__212 < t840
            if t841 {
                var t842 int
                var inline902 int = vec_len__Vec_5uint8(reversed__209)
                t842 = inline902
                var t843 int = t842 - offset__212
                var t844 int = t843 - 1
                var t845 uint8 = vec_get__Vec_5uint8(reversed__209, t844)
                vec_push__Vec_5uint8(bytes__211, t845)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t846 int = compound_old358 + compound_value359
                offset__212 = t846
                continue
            } else {
                break Loop_loop839
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
