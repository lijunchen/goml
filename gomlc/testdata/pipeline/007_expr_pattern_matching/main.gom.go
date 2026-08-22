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

type Expr interface {
    isExpr()
}

type Zero struct {}

func (_ Zero) isExpr() {}

type Succ struct {
    _0 Expr
}

func (_ Succ) isExpr() {}

type Add struct {
    _0 Expr
    _1 Expr
}

func (_ Add) isExpr() {}

type Mul struct {
    _0 Expr
    _1 Expr
}

func (_ Mul) isExpr() {}

func main0() struct{} {
    var x800 Expr = Zero{}
    switch x800.(type) {
    case Zero:
        var inline901 int = 3
        var inline902 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline901)
        _goml_runtime_core_string_print(inline902)
        return struct{}{}
    default:
        var inline905 int = 4
        var inline906 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline905)
        _goml_runtime_core_string_print(inline906)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline911 int64 = int64(int(self__404))
    var inline912 string = signed_decimal_string(inline911)
    return inline912
}

func signed_decimal_string(value__214 int64) string {
    var t865 bool = value__214 < 0
    if t865 {
        var t866 uint64 = uint64(int64(value__214))
        var t867 uint64 = 0 - t866
        var t868 string = decimal_string(t867)
        var t869 string = "-" + t868
        return t869
    } else {
        var t870 uint64 = uint64(int64(value__214))
        var t871 string = decimal_string(t870)
        return t871
    }
}

func decimal_string(value__208 uint64) string {
    var t894 bool = value__208 == 0
    if t894 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop887:
        for {
            var t888 bool = remaining__210 > 0
            if t888 {
                var t889_rhs uint64 = 10
                var t889 uint64 = remaining__210 % t889_rhs
                var t890 uint8 = uint8(uint64(t889))
                var t891 uint8 = t890 + 48
                vec_push__Vec_5uint8(reversed__209, t891)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t892 uint64 = compound_old353 / compound_value354
                remaining__210 = t892
                continue
            } else {
                break Loop_loop887
            }
        }
        var t876 int
        var inline930 int = vec_len__Vec_5uint8(reversed__209)
        t876 = inline930
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t876)
        var offset__212 int = 0
        Loop_loop878:
        for {
            var t879 int
            var inline928 int = vec_len__Vec_5uint8(reversed__209)
            t879 = inline928
            var t880 bool = offset__212 < t879
            if t880 {
                var t881 int
                var inline926 int = vec_len__Vec_5uint8(reversed__209)
                t881 = inline926
                var t882 int = t881 - offset__212
                var t883 int = t882 - 1
                var t884 uint8 = vec_get__Vec_5uint8(reversed__209, t883)
                vec_push__Vec_5uint8(bytes__211, t884)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t885 int = compound_old358 + compound_value359
                offset__212 = t885
                continue
            } else {
                break Loop_loop878
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
