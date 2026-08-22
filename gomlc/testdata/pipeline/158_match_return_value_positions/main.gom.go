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

type closure_env_f_0 struct {}

type Ordering int32

func add_after_match(flag__0 bool) int32 {
    var jp806 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp806 = 7
        var t807 int32 = jp806 + 1
        return t807
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t818 int32 = add_after_match(false)
    var inline934 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t818)
    _goml_runtime_core_string_println(inline934)
    var t819 int32
    var inline928 bool = true
    var inline930 int32
    switch inline928 {
    case true:
        t819 = 5
        var inline925 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t819)
        _goml_runtime_core_string_println(inline925)
        var t820 string
        var inline920 bool = false
        var inline922 int
        switch inline920 {
        case true:
            t820 = "early"
            var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline917)
            var t821 string
            var inline912 bool = true
            var inline914 int
            switch inline912 {
            case true:
                t821 = "early"
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            case false:
                inline914 = 7
                var inline915 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline914)
                t821 = inline915
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline922 = 7
            var inline923 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline922)
            t820 = inline923
            var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline917)
            var t821 string
            var inline912 bool = true
            var inline914 int
            switch inline912 {
            case true:
                t821 = "early"
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            case false:
                inline914 = 7
                var inline915 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline914)
                t821 = inline915
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline930 = 7
        var inline932 int32 = inline930 + 1
        t819 = inline932
        var inline925 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t819)
        _goml_runtime_core_string_println(inline925)
        var t820 string
        var inline920 bool = false
        var inline922 int
        switch inline920 {
        case true:
            t820 = "early"
            var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline917)
            var t821 string
            var inline912 bool = true
            var inline914 int
            switch inline912 {
            case true:
                t821 = "early"
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            case false:
                inline914 = 7
                var inline915 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline914)
                t821 = inline915
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline922 = 7
            var inline923 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline922)
            t820 = inline923
            var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
            _goml_runtime_core_string_println(inline917)
            var t821 string
            var inline912 bool = true
            var inline914 int
            switch inline912 {
            case true:
                t821 = "early"
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            case false:
                inline914 = 7
                var inline915 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline914)
                t821 = inline915
                var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
                _goml_runtime_core_string_println(inline909)
                var t822 int32
                var inline904 bool = false
                var inline905 closure_env_f_0 = closure_env_f_0{}
                var inline906 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline905, p0)
                }
                var inline907 int32 = inline906(inline904)
                t822 = inline907
                var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t822)
                _goml_runtime_core_string_println(inline901)
                var t823 int32
                var inline896 bool = true
                var inline897 closure_env_f_0 = closure_env_f_0{}
                var inline898 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline897, p0)
                }
                var inline899 int32 = inline898(inline896)
                t823 = inline899
                var inline893 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t823)
                _goml_runtime_core_string_println(inline893)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline937 int64 = int64(int(self__285))
    var inline938 string = signed_decimal_string(inline937)
    return inline938
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline951 int64 = int64(int32(self__407))
    var inline952 string = signed_decimal_string(inline951)
    return inline952
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t846 bool = value__214 < 0
    if t846 {
        var t847 uint64 = uint64(int64(value__214))
        var t848 uint64 = 0 - t847
        var t849 string = decimal_string(t848)
        var t850 string = "-" + t849
        return t850
    } else {
        var t851 uint64 = uint64(int64(value__214))
        var t852 string = decimal_string(t851)
        return t852
    }
}

func decimal_string(value__208 uint64) string {
    var t879 bool = value__208 == 0
    if t879 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop872:
        for {
            var t873 bool = remaining__210 > 0
            if t873 {
                var t874_rhs uint64 = 10
                var t874 uint64 = remaining__210 % t874_rhs
                var t875 uint8 = uint8(uint64(t874))
                var t876 uint8 = t875 + 48
                vec_push__Vec_5uint8(reversed__209, t876)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t877 uint64 = compound_old353 / compound_value354
                remaining__210 = t877
                continue
            } else {
                break Loop_loop872
            }
        }
        var t861 int
        var inline970 int = vec_len__Vec_5uint8(reversed__209)
        t861 = inline970
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t861)
        var offset__212 int = 0
        Loop_loop863:
        for {
            var t864 int
            var inline968 int = vec_len__Vec_5uint8(reversed__209)
            t864 = inline968
            var t865 bool = offset__212 < t864
            if t865 {
                var t866 int
                var inline966 int = vec_len__Vec_5uint8(reversed__209)
                t866 = inline966
                var t867 int = t866 - offset__212
                var t868 int = t867 - 1
                var t869 uint8 = vec_get__Vec_5uint8(reversed__209, t868)
                vec_push__Vec_5uint8(bytes__211, t869)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t870 int = compound_old358 + compound_value359
                offset__212 = t870
                continue
            } else {
                break Loop_loop863
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env802 closure_env_f_0, inner__4 bool) int32 {
    var jp888 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp888 = 4
        var t889 int32 = jp888 + 3
        return t889
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
