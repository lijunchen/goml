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

type Shape struct {
    _tag int32
    _v1_0 Point
}

func _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(self__0 Point) string {
    var x797 int32 = self__0.x
    var x798 int32 = self__0.y
    var t805 string
    var inline881 string = __goml_builtin_int32_to_string(x797)
    t805 = inline881
    var prefix__3 string = "Point(" + t805
    var t806 string = prefix__3 + ", "
    var t807 string
    var inline879 string = __goml_builtin_int32_to_string(x798)
    t807 = inline879
    var t808 string = t806 + t807
    var t809 string = t808 + ")"
    return t809
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4._tag {
    case 0:
        return "Unit"
    case 1:
        var x799 Point = self__4._v1_0
        var t814 string
        var inline884 int32 = x799.x
        var inline885 int32 = x799.y
        var inline888 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline884)
        var inline889 string = "Point(" + inline888
        var inline890 string = inline889 + ", "
        var inline891 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline885)
        var inline892 string = inline890 + inline891
        var inline893 string = inline892 + ")"
        t814 = inline893
        var t815 string = "Shape::" + t814
        return t815
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t823 string
    var inline925 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t823 = inline925
    var inline922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline922)
    var unit_shape__9 Shape = Shape{
        _tag: 0,
    }
    var t824 string
    var inline920 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t824 = inline920
    var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline917)
    var t825 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Shape{
        _tag: 1,
        _v1_0: t825,
    }
    var t826 string
    var inline915 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t826 = inline915
    var inline912 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline912)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline927 int64 = int64(int32(self__286))
    var inline928 string = signed_decimal_string(inline927)
    return inline928
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t835 int64 = int64(int32(value__225))
    var inline931 bool = t835 < 0
    if inline931 {
        var inline932 uint64 = uint64(int64(t835))
        var inline933 uint64 = 0 - inline932
        var inline934 string = decimal_string(inline933)
        var inline935 string = "-" + inline934
        return inline935
    } else {
        var inline936 uint64 = uint64(int64(t835))
        var inline937 string = decimal_string(inline936)
        return inline937
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t843 bool = value__214 < 0
    if t843 {
        var t844 uint64 = uint64(int64(value__214))
        var t845 uint64 = 0 - t844
        var t846 string = decimal_string(t845)
        var t847 string = "-" + t846
        return t847
    } else {
        var t848 uint64 = uint64(int64(value__214))
        var t849 string = decimal_string(t848)
        return t849
    }
}

func decimal_string(value__208 uint64) string {
    var t872 bool = value__208 == 0
    if t872 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop865:
        for {
            var t866 bool = remaining__210 > 0
            if t866 {
                var t867_rhs uint64 = 10
                var t867 uint64 = remaining__210 % t867_rhs
                var t868 uint8 = uint8(uint64(t867))
                var t869 uint8 = t868 + 48
                vec_push__Vec_5uint8(reversed__209, t869)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t870 uint64 = compound_old353 / compound_value354
                remaining__210 = t870
                continue
            } else {
                break Loop_loop865
            }
        }
        var t854 int
        var inline947 int = vec_len__Vec_5uint8(reversed__209)
        t854 = inline947
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t854)
        var offset__212 int = 0
        Loop_loop856:
        for {
            var t857 int
            var inline945 int = vec_len__Vec_5uint8(reversed__209)
            t857 = inline945
            var t858 bool = offset__212 < t857
            if t858 {
                var t859 int
                var inline943 int = vec_len__Vec_5uint8(reversed__209)
                t859 = inline943
                var t860 int = t859 - offset__212
                var t861 int = t860 - 1
                var t862 uint8 = vec_get__Vec_5uint8(reversed__209, t861)
                vec_push__Vec_5uint8(bytes__211, t862)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t863 int = compound_old358 + compound_value359
                offset__212 = t863
                continue
            } else {
                break Loop_loop856
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
