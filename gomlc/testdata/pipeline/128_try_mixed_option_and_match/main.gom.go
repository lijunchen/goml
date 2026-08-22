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
    _v1_0 int32
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp796 Option__i32
    if primary__2 {
        var inline897 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 4,
        }
        mtmp796 = inline897
    } else {
        mtmp796 = Option__i32{
            _tag: 0,
        }
    }
    var jp817 int32
    switch mtmp796._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x797 int32 = mtmp796._v1_0
        jp817 = x797
        var mtmp798 Option__i32
        if secondary__3 {
            var inline895 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: 9,
            }
            mtmp798 = inline895
        } else {
            mtmp798 = Option__i32{
                _tag: 0,
            }
        }
        var jp819 string
        switch mtmp798._tag {
        case 0:
            jp819 = "extra=none"
        case 1:
            var x799 int32 = mtmp798._v1_0
            var t825 string
            var inline891 string = __goml_builtin_int32_to_string(x799)
            t825 = inline891
            var t826 string = "extra=" + t825
            jp819 = t826
        default:
            panic("non-exhaustive match")
        }
        var t820 string
        var inline893 string = __goml_builtin_int32_to_string(jp817)
        t820 = inline893
        var t821 string = "value=" + t820
        var t822 string = t821 + ","
        var t823 string = t822 + jp819
        var t824 Option__string = Option__string{
            _tag: 1,
            _v1_0: t823,
        }
        return t824
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t833 Option__string = mixed(true, true)
    var t834 string
    switch t833._tag {
    case 0:
        t834 = "none"
    case 1:
        var inline916 string = t833._v1_0
        var inline918 string = "some=" + inline916
        t834 = inline918
    default:
        panic("non-exhaustive match")
    }
    var inline913 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t834)
    _goml_runtime_core_string_println(inline913)
    var t835 Option__string = mixed(true, false)
    var t836 string
    switch t835._tag {
    case 0:
        t836 = "none"
    case 1:
        var inline909 string = t835._v1_0
        var inline911 string = "some=" + inline909
        t836 = inline911
    default:
        panic("non-exhaustive match")
    }
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t836)
    _goml_runtime_core_string_println(inline906)
    var t837 Option__string = mixed(false, true)
    var t838 string
    switch t837._tag {
    case 0:
        t838 = "none"
    case 1:
        var inline902 string = t837._v1_0
        var inline904 string = "some=" + inline902
        t838 = inline904
    default:
        panic("non-exhaustive match")
    }
    var inline899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t838)
    _goml_runtime_core_string_println(inline899)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t847 int64 = int64(int32(value__225))
    var inline924 bool = t847 < 0
    if inline924 {
        var inline925 uint64 = uint64(int64(t847))
        var inline926 uint64 = 0 - inline925
        var inline927 string = decimal_string(inline926)
        var inline928 string = "-" + inline927
        return inline928
    } else {
        var inline929 uint64 = uint64(int64(t847))
        var inline930 string = decimal_string(inline929)
        return inline930
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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
        var inline940 int = vec_len__Vec_5uint8(reversed__209)
        t866 = inline940
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t866)
        var offset__212 int = 0
        Loop_loop868:
        for {
            var t869 int
            var inline938 int = vec_len__Vec_5uint8(reversed__209)
            t869 = inline938
            var t870 bool = offset__212 < t869
            if t870 {
                var t871 int
                var inline936 int = vec_len__Vec_5uint8(reversed__209)
                t871 = inline936
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

func main() {
    main0()
}
