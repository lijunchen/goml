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

type Point__i32__string struct {
    x int32
    y string
}

type Point__string__string struct {
    x string
    y string
}

type Point__string__i32 struct {
    x string
    y int32
}

type Ordering int32

func main0() struct{} {
    var p1__4 Point__i32__string
    var inline884 int32 = 10
    var inline885 string = "hello"
    var inline886 Point__i32__string = Point__i32__string{
        x: inline884,
        y: inline885,
    }
    p1__4 = inline886
    var p2__5 Point__string__string
    var inline880 string = "goml"
    var inline881 string = "lang"
    var inline882 Point__string__string = Point__string__string{
        x: inline880,
        y: inline881,
    }
    p2__5 = inline882
    var p3__6 Point__string__i32
    var inline876 string = p1__4.y
    var inline877 int32 = p1__4.x
    var inline878 Point__string__i32 = Point__string__i32{
        x: inline876,
        y: inline877,
    }
    p3__6 = inline878
    var x__7 int32 = p3__6.y
    var t798 string
    var inline874 string = __goml_builtin_int32_to_string(x__7)
    t798 = inline874
    var inline871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t798)
    _goml_runtime_core_string_println(inline871)
    var x2__8 string
    var inline869 string = p2__5.x
    x2__8 = inline869
    var inline866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline866)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t824 int64 = int64(int32(value__225))
    var inline892 bool = t824 < 0
    if inline892 {
        var inline893 uint64 = uint64(int64(t824))
        var inline894 uint64 = 0 - inline893
        var inline895 string = decimal_string(inline894)
        var inline896 string = "-" + inline895
        return inline896
    } else {
        var inline897 uint64 = uint64(int64(t824))
        var inline898 string = decimal_string(inline897)
        return inline898
    }
}

func decimal_string(value__208 uint64) string {
    var t859 bool = value__208 == 0
    if t859 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop852:
        for {
            var t853 bool = remaining__210 > 0
            if t853 {
                var t854_rhs uint64 = 10
                var t854 uint64 = remaining__210 % t854_rhs
                var t855 uint8 = uint8(uint64(t854))
                var t856 uint8 = t855 + 48
                vec_push__Vec_5uint8(reversed__209, t856)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t857 uint64 = compound_old353 / compound_value354
                remaining__210 = t857
                continue
            } else {
                break Loop_loop852
            }
        }
        var t841 int
        var inline908 int = vec_len__Vec_5uint8(reversed__209)
        t841 = inline908
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t841)
        var offset__212 int = 0
        Loop_loop843:
        for {
            var t844 int
            var inline906 int = vec_len__Vec_5uint8(reversed__209)
            t844 = inline906
            var t845 bool = offset__212 < t844
            if t845 {
                var t846 int
                var inline904 int = vec_len__Vec_5uint8(reversed__209)
                t846 = inline904
                var t847 int = t846 - offset__212
                var t848 int = t847 - 1
                var t849 uint8 = vec_get__Vec_5uint8(reversed__209, t848)
                vec_push__Vec_5uint8(bytes__211, t849)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t850 int = compound_old358 + compound_value359
                offset__212 = t850
                continue
            } else {
                break Loop_loop843
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
