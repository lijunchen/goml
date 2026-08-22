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

type Message interface {
    isMessage()
}

type Quit struct {}

func (_ Quit) isMessage() {}

type Move struct {
    _0 int32
    _1 int32
}

func (_ Move) isMessage() {}

type Write struct {
    _0 string
}

func (_ Write) isMessage() {}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    switch self__3.(type) {
    case Quit:
        return "Message::Quit"
    case Move:
        var x799 int32 = self__3.(Move)._0
        var x800 int32 = self__3.(Move)._1
        var t820 string
        var inline889 string = __goml_builtin_int32_to_string(x799)
        t820 = inline889
        var t821 string = "Message::Move(" + t820
        var t822 string = t821 + ", "
        var t823 string
        var inline887 string = __goml_builtin_int32_to_string(x800)
        t823 = inline887
        var t824 string = t822 + t823
        var t825 string = t824 + ")"
        return t825
    case Write:
        var x801 string = self__3.(Write)._0
        var t826 string = "Message::Write(" + x801
        var t827 string = t826 + ")"
        return t827
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline904 int32 = 4
    var inline905 int32 = 7
    var inline908 string = "Point { " + "x: "
    var inline909 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline904)
    var inline910 string = inline908 + inline909
    var inline911 string = inline910 + ", "
    var inline912 string = inline911 + "y: "
    var inline913 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline905)
    var inline914 string = inline912 + inline913
    var inline915 string = inline914 + " }"
    summary__8 = inline915
    var t829 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t829)
    var t830 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t830)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline900 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline900)
    var inline897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline897)
    var inline894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline894)
    var inline891 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline891)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline917 int64 = int64(int32(self__407))
    var inline918 string = signed_decimal_string(inline917)
    return inline918
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t839 int64 = int64(int32(value__225))
    var inline921 bool = t839 < 0
    if inline921 {
        var inline922 uint64 = uint64(int64(t839))
        var inline923 uint64 = 0 - inline922
        var inline924 string = decimal_string(inline923)
        var inline925 string = "-" + inline924
        return inline925
    } else {
        var inline926 uint64 = uint64(int64(t839))
        var inline927 string = decimal_string(inline926)
        return inline927
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t847 bool = value__214 < 0
    if t847 {
        var t848 uint64 = uint64(int64(value__214))
        var t849 uint64 = 0 - t848
        var t850 string = decimal_string(t849)
        var t851 string = "-" + t850
        return t851
    } else {
        var t852 uint64 = uint64(int64(value__214))
        var t853 string = decimal_string(t852)
        return t853
    }
}

func decimal_string(value__208 uint64) string {
    var t876 bool = value__208 == 0
    if t876 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop869:
        for {
            var t870 bool = remaining__210 > 0
            if t870 {
                var t871_rhs uint64 = 10
                var t871 uint64 = remaining__210 % t871_rhs
                var t872 uint8 = uint8(uint64(t871))
                var t873 uint8 = t872 + 48
                vec_push__Vec_5uint8(reversed__209, t873)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t874 uint64 = compound_old353 / compound_value354
                remaining__210 = t874
                continue
            } else {
                break Loop_loop869
            }
        }
        var t858 int
        var inline937 int = vec_len__Vec_5uint8(reversed__209)
        t858 = inline937
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t858)
        var offset__212 int = 0
        Loop_loop860:
        for {
            var t861 int
            var inline935 int = vec_len__Vec_5uint8(reversed__209)
            t861 = inline935
            var t862 bool = offset__212 < t861
            if t862 {
                var t863 int
                var inline933 int = vec_len__Vec_5uint8(reversed__209)
                t863 = inline933
                var t864 int = t863 - offset__212
                var t865 int = t864 - 1
                var t866 uint8 = vec_get__Vec_5uint8(reversed__209, t865)
                vec_push__Vec_5uint8(bytes__211, t866)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t867 int = compound_old358 + compound_value359
                offset__212 = t867
                continue
            } else {
                break Loop_loop860
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
