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

type closure_env_f_0 struct {}

type Ordering int32

func early(x__0 int32) int32 {
    var t820 bool = x__0 < 0
    if t820 {
        return 0
    } else {
        var t819 bool = x__0 == 0
        if t819 {
            return 1
        } else {
            var t818 int32 = x__0 + 2
            return t818
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t828 int32 = early(-1)
    println__T_i32(t828)
    var inline944 string = "e0: "
    var inline945 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline944)
    _goml_runtime_core_string_print(inline945)
    var t829 int32 = early(0)
    var inline941 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t829)
    _goml_runtime_core_string_println(inline941)
    var inline937 string = "e3: "
    var inline938 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline937)
    _goml_runtime_core_string_print(inline938)
    var t830 int32 = early(3)
    var inline934 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t830)
    _goml_runtime_core_string_println(inline934)
    var inline930 string = "c7: "
    var inline931 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline930)
    _goml_runtime_core_string_print(inline931)
    var t831 int32
    var inline925 int32 = 7
    var inline926 closure_env_f_0 = closure_env_f_0{}
    var inline927 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline926, p0)
    }
    var inline928 int32 = inline927(inline925)
    t831 = inline928
    var inline922 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t831)
    _goml_runtime_core_string_println(inline922)
    var inline918 string = "c2: "
    var inline919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline918)
    _goml_runtime_core_string_print(inline919)
    var t832 int32
    var inline913 int32 = 2
    var inline914 closure_env_f_0 = closure_env_f_0{}
    var inline915 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline914, p0)
    }
    var inline916 int32 = inline915(inline913)
    t832 = inline916
    var inline910 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t832)
    _goml_runtime_core_string_println(inline910)
    var inline905 bool = true
    if inline905 {
        var inline900 bool = false
        if inline900 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline900 bool = false
        if inline900 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t834 string
    t834 = value__1
    _goml_runtime_core_string_println(t834)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t837 string
    t837 = value__0
    _goml_runtime_core_string_print(t837)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t840 string
    var inline950 string = __goml_builtin_int32_to_string(value__1)
    t840 = inline950
    _goml_runtime_core_string_println(t840)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline952 int64 = int64(int32(self__407))
    var inline953 string = signed_decimal_string(inline952)
    return inline953
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t849 int64 = int64(int32(value__225))
    var inline955 bool = t849 < 0
    if inline955 {
        var inline956 uint64 = uint64(int64(t849))
        var inline957 uint64 = 0 - inline956
        var inline958 string = decimal_string(inline957)
        var inline959 string = "-" + inline958
        return inline959
    } else {
        var inline960 uint64 = uint64(int64(t849))
        var inline961 string = decimal_string(inline960)
        return inline961
    }
}

func signed_decimal_string(value__214 int64) string {
    var t855 bool = value__214 < 0
    if t855 {
        var t856 uint64 = uint64(int64(value__214))
        var t857 uint64 = 0 - t856
        var t858 string = decimal_string(t857)
        var t859 string = "-" + t858
        return t859
    } else {
        var t860 uint64 = uint64(int64(value__214))
        var t861 string = decimal_string(t860)
        return t861
    }
}

func decimal_string(value__208 uint64) string {
    var t884 bool = value__208 == 0
    if t884 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop877:
        for {
            var t878 bool = remaining__210 > 0
            if t878 {
                var t879_rhs uint64 = 10
                var t879 uint64 = remaining__210 % t879_rhs
                var t880 uint8 = uint8(uint64(t879))
                var t881 uint8 = t880 + 48
                vec_push__Vec_5uint8(reversed__209, t881)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t882 uint64 = compound_old353 / compound_value354
                remaining__210 = t882
                continue
            } else {
                break Loop_loop877
            }
        }
        var t866 int
        var inline971 int = vec_len__Vec_5uint8(reversed__209)
        t866 = inline971
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t866)
        var offset__212 int = 0
        Loop_loop868:
        for {
            var t869 int
            var inline969 int = vec_len__Vec_5uint8(reversed__209)
            t869 = inline969
            var t870 bool = offset__212 < t869
            if t870 {
                var t871 int
                var inline967 int = vec_len__Vec_5uint8(reversed__209)
                t871 = inline967
                var t872 int = t871 - offset__212
                var t873 int = t872 - 1
                var t874 uint8 = vec_get__Vec_5uint8(reversed__209, t873)
                vec_push__Vec_5uint8(bytes__211, t874)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t875 int = compound_old358 + compound_value359
                offset__212 = t875
                continue
            } else {
                break Loop_loop868
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env813 closure_env_f_0, y__2 int32) int32 {
    var t894 bool = y__2 > 5
    if t894 {
        return y__2
    } else {
        var t893 int32 = y__2 + 10
        return t893
    }
}

func main() {
    main0()
}
