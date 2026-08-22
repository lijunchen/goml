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

type Point struct {
    x int32
    y int32
}

type Ordering int32

type Maybe__i32 struct {
    _tag int32
    _v0_0 int32
}

func main0() struct{} {
    var some_number__5 Maybe__i32
    var inline897 bool = true
    if inline897 {
        var inline898 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: 42,
        }
        some_number__5 = inline898
    } else {
        some_number__5 = Maybe__i32{
            _tag: 1,
        }
    }
    var none_number__6 Maybe__i32
    var inline894 bool = false
    if inline894 {
        var inline895 Maybe__i32 = Maybe__i32{
            _tag: 0,
            _v0_0: 42,
        }
        none_number__6 = inline895
    } else {
        none_number__6 = Maybe__i32{
            _tag: 1,
        }
    }
    var t815 string
    t815 = "Point"
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline890)
    var t816 string
    switch some_number__5._tag {
    case 0:
        var inline884 int32 = some_number__5._v0_0
        var inline886 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline884)
        var inline887 string = "Just(" + inline886
        var inline888 string = inline887 + ")"
        t816 = inline888
    case 1:
        t816 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline881)
    var t817 string
    switch none_number__6._tag {
    case 0:
        var inline875 int32 = none_number__6._v0_0
        var inline877 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline875)
        var inline878 string = "Just(" + inline877
        var inline879 string = inline878 + ")"
        t817 = inline879
    case 1:
        t817 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline872 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t817)
    _goml_runtime_core_string_println(inline872)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline900 int64 = int64(int32(self__286))
    var inline901 string = signed_decimal_string(inline900)
    return inline901
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t834 bool = value__214 < 0
    if t834 {
        var t835 uint64 = uint64(int64(value__214))
        var t836 uint64 = 0 - t835
        var t837 string = decimal_string(t836)
        var t838 string = "-" + t837
        return t838
    } else {
        var t839 uint64 = uint64(int64(value__214))
        var t840 string = decimal_string(t839)
        return t840
    }
}

func decimal_string(value__208 uint64) string {
    var t863 bool = value__208 == 0
    if t863 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop856:
        for {
            var t857 bool = remaining__210 > 0
            if t857 {
                var t858_rhs uint64 = 10
                var t858 uint64 = remaining__210 % t858_rhs
                var t859 uint8 = uint8(uint64(t858))
                var t860 uint8 = t859 + 48
                vec_push__Vec_5uint8(reversed__209, t860)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t861 uint64 = compound_old353 / compound_value354
                remaining__210 = t861
                continue
            } else {
                break Loop_loop856
            }
        }
        var t845 int
        var inline920 int = vec_len__Vec_5uint8(reversed__209)
        t845 = inline920
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t845)
        var offset__212 int = 0
        Loop_loop847:
        for {
            var t848 int
            var inline918 int = vec_len__Vec_5uint8(reversed__209)
            t848 = inline918
            var t849 bool = offset__212 < t848
            if t849 {
                var t850 int
                var inline916 int = vec_len__Vec_5uint8(reversed__209)
                t850 = inline916
                var t851 int = t850 - offset__212
                var t852 int = t851 - 1
                var t853 uint8 = vec_get__Vec_5uint8(reversed__209, t852)
                vec_push__Vec_5uint8(bytes__211, t853)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t854 int = compound_old358 + compound_value359
                offset__212 = t854
                continue
            } else {
                break Loop_loop847
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
