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

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func parse(flag__0 bool) Result__i32__string {
    if flag__0 {
        var t808 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: 5,
        }
        return t808
    } else {
        var t809 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: "bad-branch",
        }
        return t809
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__i32__string {
    var jp813 int32
    if flag__1 {
        var commute_field950 int32
        var commute_field952 string
        if fallback__2 {
            commute_field950 = 5
            jp813 = commute_field950
            var t814 int32 = jp813 + 1
            var t815 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: t814,
            }
            return t815
        } else {
            commute_field952 = "bad-branch"
            var t818 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: commute_field952,
            }
            return t818
        }
    } else {
        jp813 = 10
        var t814 int32 = jp813 + 1
        var t815 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: t814,
        }
        return t815
    }
}

func show(res__4 Result__i32__string) string {
    switch res__4._tag {
    case 0:
        var x799 int32 = res__4._v0_0
        var t823 string
        var inline888 string = __goml_builtin_int32_to_string(x799)
        t823 = inline888
        var t824 string = "ok=" + t823
        return t824
    case 1:
        var x800 string = res__4._v1_0
        var t825 string = "err=" + x800
        return t825
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t827 Result__i32__string = bump(true, true)
    var t828 string = show(t827)
    var inline926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t828)
    _goml_runtime_core_string_println(inline926)
    var t829 Result__i32__string = bump(true, false)
    var t830 string
    switch t829._tag {
    case 0:
        var inline918 int32 = t829._v0_0
        var inline920 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline918)
        var inline921 string = "ok=" + inline920
        t830 = inline921
    case 1:
        var inline922 string = t829._v1_0
        var inline924 string = "err=" + inline922
        t830 = inline924
    default:
        panic("non-exhaustive match")
    }
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t830)
    _goml_runtime_core_string_println(inline915)
    var t831 Result__i32__string
    var inline901 bool = false
    var inline902 bool = false
    var inline904 int32
    if inline901 {
        var inline908 Result__i32__string = parse(inline902)
        switch inline908._tag {
        case 0:
            var inline909 int32 = inline908._v0_0
            inline904 = inline909
            var inline906 int32 = inline904 + 1
            var inline907 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: inline906,
            }
            t831 = inline907
            var t832 string
            switch t831._tag {
            case 0:
                var inline893 int32 = t831._v0_0
                var inline895 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline893)
                var inline896 string = "ok=" + inline895
                t832 = inline896
            case 1:
                var inline897 string = t831._v1_0
                var inline899 string = "err=" + inline897
                t832 = inline899
            default:
                panic("non-exhaustive match")
            }
            var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t832)
            _goml_runtime_core_string_println(inline890)
            return struct{}{}
        case 1:
            var inline911 string = inline908._v1_0
            var inline913 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: inline911,
            }
            t831 = inline913
            var t832 string
            switch t831._tag {
            case 0:
                var inline893 int32 = t831._v0_0
                var inline895 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline893)
                var inline896 string = "ok=" + inline895
                t832 = inline896
            case 1:
                var inline897 string = t831._v1_0
                var inline899 string = "err=" + inline897
                t832 = inline899
            default:
                panic("non-exhaustive match")
            }
            var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t832)
            _goml_runtime_core_string_println(inline890)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline904 = 10
        var inline906 int32 = inline904 + 1
        var inline907 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: inline906,
        }
        t831 = inline907
        var t832 string
        switch t831._tag {
        case 0:
            var inline893 int32 = t831._v0_0
            var inline895 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline893)
            var inline896 string = "ok=" + inline895
            t832 = inline896
        case 1:
            var inline897 string = t831._v1_0
            var inline899 string = "err=" + inline897
            t832 = inline899
        default:
            panic("non-exhaustive match")
        }
        var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t832)
        _goml_runtime_core_string_println(inline890)
        return struct{}{}
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline929 int64 = int64(int32(self__286))
    var inline930 string = signed_decimal_string(inline929)
    return inline930
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t841 int64 = int64(int32(value__225))
    var inline933 bool = t841 < 0
    if inline933 {
        var inline934 uint64 = uint64(int64(t841))
        var inline935 uint64 = 0 - inline934
        var inline936 string = decimal_string(inline935)
        var inline937 string = "-" + inline936
        return inline937
    } else {
        var inline938 uint64 = uint64(int64(t841))
        var inline939 string = decimal_string(inline938)
        return inline939
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t849 bool = value__214 < 0
    if t849 {
        var t850 uint64 = uint64(int64(value__214))
        var t851 uint64 = 0 - t850
        var t852 string = decimal_string(t851)
        var t853 string = "-" + t852
        return t853
    } else {
        var t854 uint64 = uint64(int64(value__214))
        var t855 string = decimal_string(t854)
        return t855
    }
}

func decimal_string(value__208 uint64) string {
    var t878 bool = value__208 == 0
    if t878 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop871:
        for {
            var t872 bool = remaining__210 > 0
            if t872 {
                var t873_rhs uint64 = 10
                var t873 uint64 = remaining__210 % t873_rhs
                var t874 uint8 = uint8(uint64(t873))
                var t875 uint8 = t874 + 48
                vec_push__Vec_5uint8(reversed__209, t875)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t876 uint64 = compound_old353 / compound_value354
                remaining__210 = t876
                continue
            } else {
                break Loop_loop871
            }
        }
        var t860 int
        var inline949 int = vec_len__Vec_5uint8(reversed__209)
        t860 = inline949
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t860)
        var offset__212 int = 0
        Loop_loop862:
        for {
            var t863 int
            var inline947 int = vec_len__Vec_5uint8(reversed__209)
            t863 = inline947
            var t864 bool = offset__212 < t863
            if t864 {
                var t865 int
                var inline945 int = vec_len__Vec_5uint8(reversed__209)
                t865 = inline945
                var t866 int = t865 - offset__212
                var t867 int = t866 - 1
                var t868 uint8 = vec_get__Vec_5uint8(reversed__209, t867)
                vec_push__Vec_5uint8(bytes__211, t868)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t869 int = compound_old358 + compound_value359
                offset__212 = t869
                continue
            } else {
                break Loop_loop862
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
