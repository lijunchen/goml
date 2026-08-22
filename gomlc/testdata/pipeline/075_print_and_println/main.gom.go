package main

import (
    _goml_os "os"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
    return struct{}{}
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

type S struct {
    value int32
}

type Ordering int32

type dyn__ToString_vtable struct {
    to_string func(any) string
}

type dyn__ToString struct {
    data any
    vtable *dyn__ToString_vtable
}

func dyn__ToString__wrap__S__to_string(self any) string {
    return _goml_m_trait__impl_i_ToString_i_S_i_to__string(self.(S))
}

func dyn__ToString__vtable__S() *dyn__ToString_vtable {
    return &dyn__ToString_vtable{
        to_string: dyn__ToString__wrap__S__to_string,
    }
}

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var t809 int32 = self__0.value
    var t810 string
    var inline922 string = __goml_builtin_int32_to_string(t809)
    t810 = inline922
    var t811 string = "S(" + t810
    var t812 string = t811 + ")"
    return t812
}

func main0() struct{} {
    var inline968 int = 1
    var inline969 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline968)
    _goml_runtime_core_string_println(inline969)
    var inline964 bool = true
    var inline965 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline964)
    _goml_runtime_core_string_println(inline965)
    var inline960 string = "hi"
    var inline961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline960)
    _goml_runtime_core_string_println(inline961)
    var inline956 struct{} = struct{}{}
    var inline957 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline956)
    _goml_runtime_core_string_println(inline957)
    var t814 string
    var inline953 int = 2
    var inline954 string = __goml_builtin_int_to_string(inline953)
    t814 = inline954
    var inline950 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline950)
    var t815 string
    var inline947 int = 2
    var inline948 string = __goml_builtin_int_to_string(inline947)
    t815 = inline948
    var inline944 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline944)
    var s__1 S = S{
        value: 9,
    }
    var inline941 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline941)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline938 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline938)
    var r__3 *ref_int_x
    var inline935 int = 5
    var inline936 *ref_int_x = ref__Ref_3int(inline935)
    r__3 = inline936
    var inline932 string = _goml_m_trait__impl_i_ToString_i_Ref_l_isize_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline932)
    var inline928 string = "no-newline"
    var inline929 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline928)
    _goml_runtime_core_string_print(inline929)
    var inline924 string = "!"
    var inline925 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline924)
    _goml_runtime_core_string_println(inline925)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline982 int64 = int64(int(self__404))
    var inline983 string = signed_decimal_string(inline982)
    return inline983
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t854 int64 = int64(int32(value__225))
    var inline999 bool = t854 < 0
    if inline999 {
        var inline1000 uint64 = uint64(int64(t854))
        var inline1001 uint64 = 0 - inline1000
        var inline1002 string = decimal_string(inline1001)
        var inline1003 string = "-" + inline1002
        return inline1003
    } else {
        var inline1004 uint64 = uint64(int64(t854))
        var inline1005 string = decimal_string(inline1004)
        return inline1005
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t858 string = _goml_runtime_core_bool_to_string(self__401)
    return t858
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__400 struct{}) string {
    var t863 string = _goml_runtime_core_unit_to_string(self__400)
    return t863
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t866 int64 = int64(int(value__222))
    var inline1007 bool = t866 < 0
    if inline1007 {
        var inline1008 uint64 = uint64(int64(t866))
        var inline1009 uint64 = 0 - inline1008
        var inline1010 string = decimal_string(inline1009)
        var inline1011 string = "-" + inline1010
        return inline1011
    } else {
        var inline1012 uint64 = uint64(int64(t866))
        var inline1013 string = decimal_string(inline1012)
        return inline1013
    }
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_isize_r__i_to__string(self__503 *ref_int_x) string {
    var v__504 int
    var inline1017 int = ref_get__Ref_3int(self__503)
    v__504 = inline1017
    var t870 string
    var inline1015 string = __goml_builtin_int_to_string(v__504)
    t870 = inline1015
    var t871 string = "ref(" + t870
    var t872 string = t871 + ")"
    return t872
}

func signed_decimal_string(value__214 int64) string {
    var t877 bool = value__214 < 0
    if t877 {
        var t878 uint64 = uint64(int64(value__214))
        var t879 uint64 = 0 - t878
        var t880 string = decimal_string(t879)
        var t881 string = "-" + t880
        return t881
    } else {
        var t882 uint64 = uint64(int64(value__214))
        var t883 string = decimal_string(t882)
        return t883
    }
}

func decimal_string(value__208 uint64) string {
    var t909 bool = value__208 == 0
    if t909 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop902:
        for {
            var t903 bool = remaining__210 > 0
            if t903 {
                var t904_rhs uint64 = 10
                var t904 uint64 = remaining__210 % t904_rhs
                var t905 uint8 = uint8(uint64(t904))
                var t906 uint8 = t905 + 48
                vec_push__Vec_5uint8(reversed__209, t906)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t907 uint64 = compound_old353 / compound_value354
                remaining__210 = t907
                continue
            } else {
                break Loop_loop902
            }
        }
        var t891 int
        var inline1027 int = vec_len__Vec_5uint8(reversed__209)
        t891 = inline1027
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t891)
        var offset__212 int = 0
        Loop_loop893:
        for {
            var t894 int
            var inline1025 int = vec_len__Vec_5uint8(reversed__209)
            t894 = inline1025
            var t895 bool = offset__212 < t894
            if t895 {
                var t896 int
                var inline1023 int = vec_len__Vec_5uint8(reversed__209)
                t896 = inline1023
                var t897 int = t896 - offset__212
                var t898 int = t897 - 1
                var t899 uint8 = vec_get__Vec_5uint8(reversed__209, t898)
                vec_push__Vec_5uint8(bytes__211, t899)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t900 int = compound_old358 + compound_value359
                offset__212 = t900
                continue
            } else {
                break Loop_loop893
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
