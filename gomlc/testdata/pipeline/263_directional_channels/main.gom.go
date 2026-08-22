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

type Tuple2_11Sender_3int_13Receiver_3int struct {
    _0 chan<- int
    _1 <-chan int
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
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

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var channel__0 chan int
    var inline925 int = 2
    var inline926 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline925)
    channel__0 = inline926
    var mtmp796 Tuple2_11Sender_3int_13Receiver_3int
    var inline923 Tuple2_11Sender_3int_13Receiver_3int = func(p0 chan int) Tuple2_11Sender_3int_13Receiver_3int {
        return Tuple2_11Sender_3int_13Receiver_3int{
            _0: p0,
            _1: p0,
        }
    }(channel__0)
    mtmp796 = inline923
    var x797 chan<- int = mtmp796._0
    var x798 <-chan int = mtmp796._1
    var inline920 int = 7
    func(p0 chan<- int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(x797, inline920)
    var _goml_m_value____3_i_select__value int
    var _goml_m_value____3_i_select__open bool
    var value__3 Option__isize = Option__isize{
        _tag: 0,
    }
    select {
    case _goml_m_value____3_i_select__value, _goml_m_value____3_i_select__open = <-x798:
        if _goml_m_value____3_i_select__open {
            value__3 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____3_i_select__value,
            }
        }
        var t808 int
        var inline887 int = 0
        switch value__3._tag {
        case 0:
            t808 = inline887
        case 1:
            var inline888 int = value__3._v1_0
            t808 = inline888
        default:
            panic("non-exhaustive match")
        }
        var inline884 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t808)
        _goml_runtime_core_string_println(inline884)
    default:
        var inline891 int = 1
        var inline892 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline891)
        _goml_runtime_core_string_println(inline892)
    }
    var channel__4 chan int
    var inline917 int = 1
    var inline918 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline917)
    channel__4 = inline918
    var sender__5 chan<- int
    var inline915 chan<- int = func(p0 chan int) chan<- int {
        return p0
    }(channel__4)
    sender__5 = inline915
    var receiver__6 <-chan int
    var inline913 <-chan int = func(p0 chan int) <-chan int {
        return p0
    }(channel__4)
    receiver__6 = inline913
    select {
    case sender__5 <- 9:
        var t804 Option__isize
        var inline902 Tuple2_3int_4bool = func(p0 <-chan int) Tuple2_3int_4bool {
            var value int
            var ok bool
            value, ok = <-p0
            return Tuple2_3int_4bool{
                _0: value,
                _1: ok,
            }
        }(receiver__6)
        var inline903 int = inline902._0
        var inline904 bool = inline902._1
        if inline904 {
            var inline907 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: inline903,
            }
            t804 = inline907
        } else {
            t804 = Option__isize{
                _tag: 0,
            }
        }
        var t805 int
        var inline898 int = 0
        switch t804._tag {
        case 0:
            t805 = inline898
        case 1:
            var inline899 int = t804._v1_0
            t805 = inline899
        default:
            panic("non-exhaustive match")
        }
        var inline895 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t805)
        _goml_runtime_core_string_println(inline895)
        return struct{}{}
    default:
        var inline909 int = 2
        var inline910 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline909)
        _goml_runtime_core_string_println(inline910)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline930 int64 = int64(int(self__404))
    var inline931 string = signed_decimal_string(inline930)
    return inline931
}

func signed_decimal_string(value__214 int64) string {
    var t848 bool = value__214 < 0
    if t848 {
        var t849 uint64 = uint64(int64(value__214))
        var t850 uint64 = 0 - t849
        var t851 string = decimal_string(t850)
        var t852 string = "-" + t851
        return t852
    } else {
        var t853 uint64 = uint64(int64(value__214))
        var t854 string = decimal_string(t853)
        return t854
    }
}

func decimal_string(value__208 uint64) string {
    var t877 bool = value__208 == 0
    if t877 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop870:
        for {
            var t871 bool = remaining__210 > 0
            if t871 {
                var t872_rhs uint64 = 10
                var t872 uint64 = remaining__210 % t872_rhs
                var t873 uint8 = uint8(uint64(t872))
                var t874 uint8 = t873 + 48
                vec_push__Vec_5uint8(reversed__209, t874)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t875 uint64 = compound_old353 / compound_value354
                remaining__210 = t875
                continue
            } else {
                break Loop_loop870
            }
        }
        var t859 int
        var inline949 int = vec_len__Vec_5uint8(reversed__209)
        t859 = inline949
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t859)
        var offset__212 int = 0
        Loop_loop861:
        for {
            var t862 int
            var inline947 int = vec_len__Vec_5uint8(reversed__209)
            t862 = inline947
            var t863 bool = offset__212 < t862
            if t863 {
                var t864 int
                var inline945 int = vec_len__Vec_5uint8(reversed__209)
                t864 = inline945
                var t865 int = t864 - offset__212
                var t866 int = t865 - 1
                var t867 uint8 = vec_get__Vec_5uint8(reversed__209, t866)
                vec_push__Vec_5uint8(bytes__211, t867)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t868 int = compound_old358 + compound_value359
                offset__212 = t868
                continue
            } else {
                break Loop_loop861
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
