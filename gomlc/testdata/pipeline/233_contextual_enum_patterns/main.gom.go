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

type _goml_vec_Boxed__isize struct {
    items []Boxed__isize
}

func vec_get__Vec_12Boxed__isize(vec *_goml_vec_Boxed__isize, index int) Boxed__isize {
    return vec.items[index]
}

func vec_len__Vec_12Boxed__isize(vec *_goml_vec_Boxed__isize) int {
    return int(len(vec.items))
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

type ref_Option__isize_x struct {
    value Option__isize
}

func ref__Ref_13Option__isize(value Option__isize) *ref_Option__isize_x {
    return &ref_Option__isize_x{
        value: value,
    }
}

func ref_get__Ref_13Option__isize(reference *ref_Option__isize_x) Option__isize {
    return reference.value
}

func ref_set__Ref_13Option__isize(reference *ref_Option__isize_x, value Option__isize) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
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

type Second struct {
    _tag int32
    _v0_0 int
}

type First__isize interface {
    isFirst__isize()
}

type First__isize_Shared struct {
    _0 int
}

func (_ First__isize_Shared) isFirst__isize() {}

type Idle struct {}

func (_ Idle) isFirst__isize() {}

type Data struct {
    _0 int
    _1 string
}

func (_ Data) isFirst__isize() {}

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Option__Result__isize__string struct {
    _tag int32
    _v1_0 Result__isize__string
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Boxed__isize struct {
    _tag int32
    _v0_0 int
}

func classify(value__0 First__isize) string {
    switch value__0.(type) {
    case First__isize_Shared:
        var x796 int = value__0.(First__isize_Shared)._0
        var t837 string
        var inline1008 string = __goml_builtin_int_to_string(x796)
        t837 = inline1008
        var t838 string = "shared:" + t837
        return t838
    case Idle:
        return "idle"
    case Data:
        var x797 int = value__0.(Data)._0
        var x798 string = value__0.(Data)._1
        var t839 string = x798 + ":"
        var t840 string
        var inline1010 string = __goml_builtin_int_to_string(x797)
        t840 = inline1010
        var t841 string = t839 + t840
        return t841
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__isize__string) string {
    switch value__4._tag {
    case 0:
        return "none"
    case 1:
        var x799 Result__isize__string = value__4._v1_0
        switch x799._tag {
        case 0:
            var x800 int = x799._v0_0
            var t848 string
            var inline1012 string = __goml_builtin_int_to_string(x800)
            t848 = inline1012
            var t849 string = "ok:" + t848
            return t849
        case 1:
            var x801 string = x799._v1_0
            var t850 string = "err:" + x801
            return t850
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__isize) int {
    var current__11 *ref_Option__isize_x
    var inline1025 *ref_Option__isize_x = ref__Ref_13Option__isize(value__10)
    current__11 = inline1025
    var result__12 *ref_int_x
    var inline1022 int = 0
    var inline1023 *ref_int_x = ref__Ref_3int(inline1022)
    result__12 = inline1023
    Loop_loop863:
    for {
        var mtmp806 Option__isize
        var inline1018 Option__isize = ref_get__Ref_13Option__isize(current__11)
        mtmp806 = inline1018
        switch mtmp806._tag {
        case 1:
            var x807 int = mtmp806._v1_0
            ref_set__Ref_3int(result__12, x807)
            ref_set__Ref_13Option__isize(current__11, Option__isize{
                _tag: 0,
            })
            continue
        default:
            break Loop_loop863
        }
    }
    var inline1020 int = ref_get__Ref_3int(result__12)
    return inline1020
}

func sum_boxed(values__16 *_goml_vec_Boxed__isize) int {
    var result__17 *ref_int_x
    var inline1033 int = 0
    var inline1034 *ref_int_x = ref__Ref_3int(inline1033)
    result__17 = inline1034
    var for_limit814 int = vec_len__Vec_12Boxed__isize(values__16)
    var for_index815 int = 0
    Loop_loop873:
    for {
        var t874 bool = for_index815 < for_limit814
        if t874 {
            var for_item816 Boxed__isize = vec_get__Vec_12Boxed__isize(values__16, for_index815)
            var t875 int = for_index815 + 1
            for_index815 = t875
            switch for_item816._tag {
            case 0:
                var x818 int = for_item816._v0_0
                var t877 int
                var inline1029 int = ref_get__Ref_3int(result__17)
                t877 = inline1029
                var t878 int = t877 + x818
                ref_set__Ref_3int(result__17, t878)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop873
        }
    }
    var inline1031 int = ref_get__Ref_3int(result__17)
    return inline1031
}

func main0() struct{} {
    var t880 Boxed__isize = Boxed__isize{
        _tag: 0,
        _v0_0: 19,
    }
    var t881 Boxed__isize = Boxed__isize{
        _tag: 0,
        _v0_0: 23,
    }
    var t882 [2]Boxed__isize = [2]Boxed__isize{t880, t881}
    var boxed__19 *_goml_vec_Boxed__isize = func(values [2]Boxed__isize) *_goml_vec_Boxed__isize {
        var storage struct {
            vector _goml_vec_Boxed__isize
            values [2]Boxed__isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t882)
    var t883 First__isize = First__isize_Shared{
        _0: 7,
    }
    var t884 string = classify(t883)
    println__T_string(t884)
    var t885 string = classify(Idle{})
    println__T_string(t885)
    var t886 First__isize = Data{
        _0: 9,
        _1: "data",
    }
    var t887 string = classify(t886)
    println__T_string(t887)
    var t888 Result__isize__string = Result__isize__string{
        _tag: 0,
        _v0_0: 11,
    }
    var t889 Option__Result__isize__string = Option__Result__isize__string{
        _tag: 1,
        _v1_0: t888,
    }
    var t890 string = nested(t889)
    println__T_string(t890)
    var t891 Result__isize__string = Result__isize__string{
        _tag: 1,
        _v1_0: "bad",
    }
    var t892 Option__Result__isize__string = Option__Result__isize__string{
        _tag: 1,
        _v1_0: t891,
    }
    var t893 string = nested(t892)
    println__T_string(t893)
    var t894 string = nested(Option__Result__isize__string{
        _tag: 0,
    })
    var inline1069 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t894)
    _goml_runtime_core_string_println(inline1069)
    var t896 int
    var inline1066 int = 13
    t896 = inline1066
    var inline1063 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t896)
    _goml_runtime_core_string_println(inline1063)
    var t897 int
    t897 = 0
    var inline1057 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t897)
    _goml_runtime_core_string_println(inline1057)
    var t898 bool
    t898 = true
    var inline1053 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t898)
    _goml_runtime_core_string_println(inline1053)
    var t900 bool
    t900 = false
    var inline1049 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t900)
    _goml_runtime_core_string_println(inline1049)
    var t901 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 15,
    }
    var t902 int = take_once(t901)
    var inline1046 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t902)
    _goml_runtime_core_string_println(inline1046)
    var t904 int
    var inline1043 int = 17
    t904 = inline1043
    var inline1039 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t904)
    _goml_runtime_core_string_println(inline1039)
    var t905 int = sum_boxed(boxed__19)
    var inline1036 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t905)
    _goml_runtime_core_string_println(inline1036)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t927 string
    t927 = value__1
    _goml_runtime_core_string_println(t927)
    return struct{}{}
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t937 int64 = int64(int(value__222))
    var inline1080 bool = t937 < 0
    if inline1080 {
        var inline1081 uint64 = uint64(int64(t937))
        var inline1082 uint64 = 0 - inline1081
        var inline1083 string = decimal_string(inline1082)
        var inline1084 string = "-" + inline1083
        return inline1084
    } else {
        var inline1085 uint64 = uint64(int64(t937))
        var inline1086 string = decimal_string(inline1085)
        return inline1086
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1088 int64 = int64(int(self__404))
    var inline1089 string = signed_decimal_string(inline1088)
    return inline1089
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t946 string = _goml_runtime_core_bool_to_string(self__401)
    return t946
}

func signed_decimal_string(value__214 int64) string {
    var t951 bool = value__214 < 0
    if t951 {
        var t952 uint64 = uint64(int64(value__214))
        var t953 uint64 = 0 - t952
        var t954 string = decimal_string(t953)
        var t955 string = "-" + t954
        return t955
    } else {
        var t956 uint64 = uint64(int64(value__214))
        var t957 string = decimal_string(t956)
        return t957
    }
}

func decimal_string(value__208 uint64) string {
    var t980 bool = value__208 == 0
    if t980 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop973:
        for {
            var t974 bool = remaining__210 > 0
            if t974 {
                var t975_rhs uint64 = 10
                var t975 uint64 = remaining__210 % t975_rhs
                var t976 uint8 = uint8(uint64(t975))
                var t977 uint8 = t976 + 48
                vec_push__Vec_5uint8(reversed__209, t977)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t978 uint64 = compound_old353 / compound_value354
                remaining__210 = t978
                continue
            } else {
                break Loop_loop973
            }
        }
        var t962 int
        var inline1099 int = vec_len__Vec_5uint8(reversed__209)
        t962 = inline1099
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t962)
        var offset__212 int = 0
        Loop_loop964:
        for {
            var t965 int
            var inline1097 int = vec_len__Vec_5uint8(reversed__209)
            t965 = inline1097
            var t966 bool = offset__212 < t965
            if t966 {
                var t967 int
                var inline1095 int = vec_len__Vec_5uint8(reversed__209)
                t967 = inline1095
                var t968 int = t967 - offset__212
                var t969 int = t968 - 1
                var t970 uint8 = vec_get__Vec_5uint8(reversed__209, t969)
                vec_push__Vec_5uint8(bytes__211, t970)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t971 int = compound_old358 + compound_value359
                offset__212 = t971
                continue
            } else {
                break Loop_loop964
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
