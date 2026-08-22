package main

import (
    _goml_os "os"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
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

type Point struct {
    x int32
    y int32
}

type Wrapper__i32 struct {
    value int32
}

type Wrapper__unit struct {
    value struct{}
}

type Ordering int32

type Shape__i32 struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__i32
}

type Shape__unit struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__unit
}

func bounce_int(shape__0 Shape__i32) Shape__i32 {
    switch shape__0._tag {
    case 0:
        var x796 Point = shape__0._v0_0
        var t825 Shape__i32 = Shape__i32{
            _tag: 0,
            _v0_0: x796,
        }
        return t825
    case 1:
        var x797 Wrapper__i32 = shape__0._v1_0
        var t826 Shape__i32 = Shape__i32{
            _tag: 1,
            _v1_0: x797,
        }
        return t826
    case 2:
        return Shape__i32{
            _tag: 2,
        }
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x801 int32 = point__8.x
    var x802 int32 = point__8.y
    var t838 string
    var inline949 string = __goml_builtin_int32_to_string(x801)
    t838 = inline949
    var with_x__11 string = "Point { x: " + t838
    var with_y_label__12 string = with_x__11 + ", y: "
    var t839 string
    var inline947 string = __goml_builtin_int32_to_string(x802)
    t839 = inline947
    var with_y__13 string = with_y_label__12 + t839
    var t840 string = with_y__13 + " }"
    return t840
}

func wrapper_int32_to_string(wrapper__14 Wrapper__i32) string {
    var x804 int32 = wrapper__14.value
    var t843 string
    var inline951 string = __goml_builtin_int32_to_string(x804)
    t843 = inline951
    var prefix__16 string = "Wrapper[i32] { value: " + t843
    var t844 string = prefix__16 + " }"
    return t844
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x806 struct{} = wrapper__17.value
    var t847 string
    var inline953 string = _goml_runtime_core_unit_to_string(x806)
    t847 = inline953
    var prefix__19 string = "Wrapper[unit] { value: " + t847
    var t848 string = prefix__19 + " }"
    return t848
}

func shape_int32_to_string(shape__20 Shape__i32) string {
    switch shape__20._tag {
    case 0:
        var x807 Point = shape__20._v0_0
        var t853 string
        var inline956 int32 = x807.x
        var inline957 int32 = x807.y
        var inline960 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline956)
        var inline961 string = "Point { x: " + inline960
        var inline962 string = inline961 + ", y: "
        var inline963 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline957)
        var inline964 string = inline962 + inline963
        var inline965 string = inline964 + " }"
        t853 = inline965
        var prefix__22 string = "Shape::Dot(" + t853
        var t854 string = prefix__22 + ")"
        return t854
    case 1:
        var x808 Wrapper__i32 = shape__20._v1_0
        var t855 string
        var inline968 int32 = x808.value
        var inline970 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline968)
        var inline971 string = "Wrapper[i32] { value: " + inline970
        var inline972 string = inline971 + " }"
        t855 = inline972
        var prefix__24 string = "Shape::Wrapped(" + t855
        var t856 string = prefix__24 + ")"
        return t856
    case 2:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25._tag {
    case 0:
        var x809 Point = shape__25._v0_0
        var t861 string
        var inline975 int32 = x809.x
        var inline976 int32 = x809.y
        var inline979 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline975)
        var inline980 string = "Point { x: " + inline979
        var inline981 string = inline980 + ", y: "
        var inline982 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline976)
        var inline983 string = inline981 + inline982
        var inline984 string = inline983 + " }"
        t861 = inline984
        var prefix__27 string = "Shape::Dot(" + t861
        var t862 string = prefix__27 + ")"
        return t862
    case 1:
        var x810 Wrapper__unit = shape__25._v1_0
        var t863 string
        var inline987 struct{} = x810.value
        var inline989 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline987)
        var inline990 string = "Wrapper[unit] { value: " + inline989
        var inline991 string = inline990 + " }"
        t863 = inline991
        var prefix__29 string = "Shape::Wrapped(" + t863
        var t864 string = prefix__29 + ")"
        return t864
    case 2:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t866 Point = Point{
        x: 3,
        y: 4,
    }
    var t867 string = point32_to_string(t866)
    println__T_string(t867)
    var t868 Wrapper__i32 = Wrapper__i32{
        value: 7,
    }
    var t869 string = wrapper_int32_to_string(t868)
    println__T_string(t869)
    var t870 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t871 string = wrapper_unit_to_string(t870)
    println__T_string(t871)
    var bounced_origin__30 Shape__i32 = bounce_int(Shape__i32{
        _tag: 2,
    })
    var t872 Point = Point{
        x: 3,
        y: 4,
    }
    var t873 Shape__i32 = Shape__i32{
        _tag: 0,
        _v0_0: t872,
    }
    var t874 string = shape_int32_to_string(t873)
    println__T_string(t874)
    var t875 Wrapper__i32 = Wrapper__i32{
        value: 7,
    }
    var t876 Shape__i32 = Shape__i32{
        _tag: 1,
        _v1_0: t875,
    }
    var t877 string = shape_int32_to_string(t876)
    var inline1028 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t877)
    _goml_runtime_core_string_println(inline1028)
    var t878 string = shape_int32_to_string(bounced_origin__30)
    var inline1025 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t878)
    _goml_runtime_core_string_println(inline1025)
    var t879 Point = Point{
        x: 3,
        y: 4,
    }
    var t880 Shape__unit = Shape__unit{
        _tag: 0,
        _v0_0: t879,
    }
    var t881 string = shape_unit_to_string(t880)
    var inline1022 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t881)
    _goml_runtime_core_string_println(inline1022)
    var t882 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t883 Shape__unit = Shape__unit{
        _tag: 1,
        _v1_0: t882,
    }
    var t884 string = shape_unit_to_string(t883)
    var inline1019 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t884)
    _goml_runtime_core_string_println(inline1019)
    var t885 string
    t885 = "Shape::Origin"
    var inline1005 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t885)
    _goml_runtime_core_string_println(inline1005)
    var t886 Shape__i32
    t886 = Shape__i32{
        _tag: 2,
    }
    switch t886._tag {
    case 0:
    case 1:
    case 2:
    default:
        panic("non-exhaustive match")
    }
    var inline993 string = "struct enums!"
    var inline994 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline993)
    _goml_runtime_core_string_println(inline994)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline1031 int64 = int64(int32(self__286))
    var inline1032 string = signed_decimal_string(inline1031)
    return inline1032
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__400 struct{}) string {
    var t893 string = _goml_runtime_core_unit_to_string(self__400)
    return t893
}

func println__T_string(value__1 string) struct{} {
    var t895 string
    t895 = value__1
    _goml_runtime_core_string_println(t895)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t903 int64 = int64(int32(value__225))
    var inline1035 bool = t903 < 0
    if inline1035 {
        var inline1036 uint64 = uint64(int64(t903))
        var inline1037 uint64 = 0 - inline1036
        var inline1038 string = decimal_string(inline1037)
        var inline1039 string = "-" + inline1038
        return inline1039
    } else {
        var inline1040 uint64 = uint64(int64(t903))
        var inline1041 string = decimal_string(inline1040)
        return inline1041
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t911 bool = value__214 < 0
    if t911 {
        var t912 uint64 = uint64(int64(value__214))
        var t913 uint64 = 0 - t912
        var t914 string = decimal_string(t913)
        var t915 string = "-" + t914
        return t915
    } else {
        var t916 uint64 = uint64(int64(value__214))
        var t917 string = decimal_string(t916)
        return t917
    }
}

func decimal_string(value__208 uint64) string {
    var t940 bool = value__208 == 0
    if t940 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop933:
        for {
            var t934 bool = remaining__210 > 0
            if t934 {
                var t935_rhs uint64 = 10
                var t935 uint64 = remaining__210 % t935_rhs
                var t936 uint8 = uint8(uint64(t935))
                var t937 uint8 = t936 + 48
                vec_push__Vec_5uint8(reversed__209, t937)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t938 uint64 = compound_old353 / compound_value354
                remaining__210 = t938
                continue
            } else {
                break Loop_loop933
            }
        }
        var t922 int
        var inline1051 int = vec_len__Vec_5uint8(reversed__209)
        t922 = inline1051
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t922)
        var offset__212 int = 0
        Loop_loop924:
        for {
            var t925 int
            var inline1049 int = vec_len__Vec_5uint8(reversed__209)
            t925 = inline1049
            var t926 bool = offset__212 < t925
            if t926 {
                var t927 int
                var inline1047 int = vec_len__Vec_5uint8(reversed__209)
                t927 = inline1047
                var t928 int = t927 - offset__212
                var t929 int = t928 - 1
                var t930 uint8 = vec_get__Vec_5uint8(reversed__209, t929)
                vec_push__Vec_5uint8(bytes__211, t930)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t931 int = compound_old358 + compound_value359
                offset__212 = t931
                continue
            } else {
                break Loop_loop924
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
