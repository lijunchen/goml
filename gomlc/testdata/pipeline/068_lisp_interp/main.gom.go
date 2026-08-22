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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_Token struct {
    items []Token
}

func vec_new__Vec_5Token() *_goml_vec_Token {
    return &_goml_vec_Token{
        items: nil,
    }
}

func vec_with_capacity__Vec_5Token(capacity int) *_goml_vec_Token {
    return &_goml_vec_Token{
        items: make([]Token, 0, capacity),
    }
}

func vec_push__Vec_5Token(vec *_goml_vec_Token, elem Token) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int {
    return int(len(vec.items))
}

type _goml_vec_Binding struct {
    items []Binding
}

func vec_new__Vec_7Binding() *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: nil,
    }
}

func vec_with_capacity__Vec_7Binding(capacity int) *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: make([]Binding, 0, capacity),
    }
}

func vec_push__Vec_7Binding(vec *_goml_vec_Binding, elem Binding) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int {
    return int(len(vec.items))
}

type _goml_vec_SExpr struct {
    items []SExpr
}

func vec_new__Vec_5SExpr() *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: nil,
    }
}

func vec_with_capacity__Vec_5SExpr(capacity int) *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: make([]SExpr, 0, capacity),
    }
}

func vec_push__Vec_5SExpr(vec *_goml_vec_SExpr, elem SExpr) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int {
    return int(len(vec.items))
}

type _goml_vec_Value struct {
    items []Value
}

func vec_new__Vec_5Value() *_goml_vec_Value {
    return &_goml_vec_Value{
        items: nil,
    }
}

func vec_with_capacity__Vec_5Value(capacity int) *_goml_vec_Value {
    return &_goml_vec_Value{
        items: make([]Value, 0, capacity),
    }
}

func vec_push__Vec_5Value(vec *_goml_vec_Value, elem Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: make([]string, 0, capacity),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Token_x struct {
    value *_goml_vec_Token
}

func ref__Ref_10Vec_5Token(value *_goml_vec_Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) *_goml_vec_Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value *_goml_vec_Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Value_x struct {
    value Value
}

func ref__Ref_5Value(value Value) *ref_Value_x {
    return &ref_Value_x{
        value: value,
    }
}

func ref_get__Ref_5Value(reference *ref_Value_x) Value {
    return reference.value
}

func ref_set__Ref_5Value(reference *ref_Value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5SExpr_x struct {
    value *_goml_vec_SExpr
}

func ref__Ref_10Vec_5SExpr(value *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value *_goml_vec_SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value *_goml_vec_Binding
}

func ref__Ref_12Vec_7Binding(value *_goml_vec_Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) *_goml_vec_Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value *_goml_vec_Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value *_goml_vec_string
}

func ref__Ref_11Vec_6string(value *_goml_vec_string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) *_goml_vec_string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value *_goml_vec_string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value *_goml_vec_Value
}

func ref__Ref_10Vec_5Value(value *_goml_vec_Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) *_goml_vec_Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value *_goml_vec_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_3int struct {
    _0 Token
    _1 int
}

type Tuple2_10Vec_5SExpr_3int struct {
    _0 *_goml_vec_SExpr
    _1 int
}

type Tuple2_5SExpr_3int struct {
    _0 SExpr
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type Binding struct {
    name string
    value Value
}

type Lambda struct {
    params *_goml_vec_string
    body SExpr
    env *_goml_vec_Binding
    global *ref_Vec_7Binding_x
}

type Ordering int32

type Token interface {
    isToken()
}

type LParen struct {}

func (_ LParen) isToken() {}

type RParen struct {}

func (_ RParen) isToken() {}

type Token_Sym struct {
    _0 string
}

func (_ Token_Sym) isToken() {}

type Token_Int struct {
    _0 int32
}

func (_ Token_Int) isToken() {}

type Token_Bool struct {
    _0 bool
}

func (_ Token_Bool) isToken() {}

type Value interface {
    isValue()
}

type Value_Int struct {
    _0 int32
}

func (_ Value_Int) isValue() {}

type Value_Bool struct {
    _0 bool
}

func (_ Value_Bool) isValue() {}

type Func struct {
    _0 Lambda
}

func (_ Func) isValue() {}

type Nil struct {}

func (_ Nil) isValue() {}

type SExpr interface {
    isSExpr()
}

type SExpr_Int struct {
    _0 int32
}

func (_ SExpr_Int) isSExpr() {}

type SExpr_Bool struct {
    _0 bool
}

func (_ SExpr_Bool) isSExpr() {}

type SExpr_Sym struct {
    _0 string
}

func (_ SExpr_Sym) isSExpr() {}

type List struct {
    _0 *_goml_vec_SExpr
}

func (_ List) isSExpr() {}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func is_int_text(text__2 string) bool {
    var len__3 int
    var inline1927 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1927
    var t946 bool = len__3 == 0
    if t946 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1924 int = 0
        var inline1925 *ref_int_x = ref__Ref_3int(inline1924)
        i__4 = inline1925
        var saw_digit__5 *ref_bool_x
        var inline1921 bool = false
        var inline1922 *ref_bool_x = ref__Ref_4bool(inline1921)
        saw_digit__5 = inline1922
        var ok__6 *ref_bool_x
        var inline1918 bool = true
        var inline1919 *ref_bool_x = ref__Ref_4bool(inline1918)
        ok__6 = inline1919
        var started__7 *ref_bool_x
        var inline1915 bool = false
        var inline1916 *ref_bool_x = ref__Ref_4bool(inline1915)
        started__7 = inline1916
        Loop_loop952:
        for {
            var t971 bool
            var inline1909 bool = ref_get__Ref_4bool(ok__6)
            t971 = inline1909
            var jp954 bool
            if t971 {
                var t972 int
                var inline1878 int = ref_get__Ref_3int(i__4)
                t972 = inline1878
                var t973 bool = t972 < len__3
                jp954 = t973
            } else {
                jp954 = false
            }
            if jp954 {
                var t955 int
                var inline1907 int = ref_get__Ref_3int(i__4)
                t955 = inline1907
                var ch__8 rune
                var inline1905 rune = string_get(text__2, t955)
                ch__8 = inline1905
                var t968 bool
                var inline1903 bool = ref_get__Ref_4bool(started__7)
                t968 = inline1903
                var t969 bool = !t968
                var jp958 bool
                if t969 {
                    var t970 bool = ch__8 == 45
                    jp958 = t970
                } else {
                    jp958 = false
                }
                if jp958 {
                    var inline1884 bool = true
                    ref_set__Ref_4bool(started__7, inline1884)
                    var t959 int
                    var inline1882 int = ref_get__Ref_3int(i__4)
                    t959 = inline1882
                    var t960 int = t959 + 1
                    ref_set__Ref_3int(i__4, t960)
                    continue
                } else {
                    var t963 bool
                    var inline1900 bool = ch__8 >= 48
                    if inline1900 {
                        var inline1901 bool = ch__8 <= 57
                        t963 = inline1901
                    } else {
                        t963 = false
                    }
                    if t963 {
                        var inline1894 bool = true
                        ref_set__Ref_4bool(started__7, inline1894)
                        var inline1891 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1891)
                        var t964 int
                        var inline1889 int = ref_get__Ref_3int(i__4)
                        t964 = inline1889
                        var t965 int = t964 + 1
                        ref_set__Ref_3int(i__4, t965)
                        continue
                    } else {
                        var inline1897 bool = false
                        ref_set__Ref_4bool(ok__6, inline1897)
                        continue
                    }
                }
            } else {
                break Loop_loop952
            }
        }
        var t950 bool
        var inline1913 bool = ref_get__Ref_4bool(ok__6)
        t950 = inline1913
        if t950 {
            var inline1911 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1911
        } else {
            return false
        }
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x
    var inline1968 bool = false
    var inline1969 *ref_bool_x = ref__Ref_4bool(inline1968)
    started__13 = inline1969
    var acc__14 *ref_int32_x
    var inline1965 int32 = 0
    var inline1966 *ref_int32_x = ref__Ref_5int32(inline1965)
    acc__14 = inline1966
    Loop_loop983:
    for {
        var t984 int
        var inline1957 int = ref_get__Ref_3int(i__11)
        t984 = inline1957
        var t985 bool = t984 < len__10
        if t985 {
            var t986 int
            var inline1955 int = ref_get__Ref_3int(i__11)
            t986 = inline1955
            var ch__15 rune
            var inline1953 rune = string_get(text__9, t986)
            ch__15 = inline1953
            var t999 bool
            var inline1951 bool = ref_get__Ref_4bool(started__13)
            t999 = inline1951
            var t1000 bool = !t999
            var jp989 bool
            if t1000 {
                var t1001 bool = ch__15 == 45
                jp989 = t1001
            } else {
                jp989 = false
            }
            if jp989 {
                var inline1936 bool = true
                ref_set__Ref_4bool(started__13, inline1936)
                var inline1933 bool = true
                ref_set__Ref_4bool(negative__12, inline1933)
                var t990 int
                var inline1931 int = ref_get__Ref_3int(i__11)
                t990 = inline1931
                var t991 int = t990 + 1
                ref_set__Ref_3int(i__11, t991)
                continue
            } else {
                var inline1948 bool = true
                ref_set__Ref_4bool(started__13, inline1948)
                var d__16 int32
                switch ch__15 {
                case 48:
                    d__16 = 0
                case 49:
                    d__16 = 1
                case 50:
                    d__16 = 2
                case 51:
                    d__16 = 3
                case 52:
                    d__16 = 4
                case 53:
                    d__16 = 5
                case 54:
                    d__16 = 6
                case 55:
                    d__16 = 7
                case 56:
                    d__16 = 8
                case 57:
                    d__16 = 9
                default:
                    d__16 = 0
                }
                var t993 int32
                var inline1945 int32 = ref_get__Ref_5int32(acc__14)
                t993 = inline1945
                var t994 int32 = t993 * 10
                var t995 int32 = t994 + d__16
                ref_set__Ref_5int32(acc__14, t995)
                var t996 int
                var inline1941 int = ref_get__Ref_3int(i__11)
                t996 = inline1941
                var t997 int = t996 + 1
                ref_set__Ref_3int(i__11, t997)
                continue
            }
        } else {
            break Loop_loop983
        }
    }
    var t979 bool
    var inline1963 bool = ref_get__Ref_4bool(negative__12)
    t979 = inline1963
    if t979 {
        var t980 int32
        var inline1959 int32 = ref_get__Ref_5int32(acc__14)
        t980 = inline1959
        var t981 int32 = 0 - t980
        return t981
    } else {
        var inline1961 int32 = ref_get__Ref_5int32(acc__14)
        return inline1961
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline2010 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline2010
    var text__21 *ref_string_x
    var inline2007 string = ""
    var inline2008 *ref_string_x = ref__Ref_6string(inline2007)
    text__21 = inline2008
    var i__22 *ref_int_x
    var inline2005 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline2005
    var done__23 *ref_bool_x
    var inline2002 bool = false
    var inline2003 *ref_bool_x = ref__Ref_4bool(inline2002)
    done__23 = inline2003
    Loop_loop1026:
    for {
        var t1039 bool
        var inline1996 bool = ref_get__Ref_4bool(done__23)
        t1039 = inline1996
        var t1040 bool = !t1039
        var jp1028 bool
        if t1040 {
            var t1041 int
            var inline1971 int = ref_get__Ref_3int(i__22)
            t1041 = inline1971
            var t1042 bool = t1041 < len__20
            jp1028 = t1042
        } else {
            jp1028 = false
        }
        if jp1028 {
            var t1029 int
            var inline1994 int = ref_get__Ref_3int(i__22)
            t1029 = inline1994
            var ch__24 rune
            var inline1992 rune = string_get(source__18, t1029)
            ch__24 = inline1992
            var t1031 bool
            var inline1986 bool = ch__24 == 40
            var inline1988 bool
            if inline1986 {
                inline1988 = true
            } else {
                var inline1990 bool = ch__24 == 41
                inline1988 = inline1990
            }
            if inline1988 {
                t1031 = true
                if t1031 {
                    var inline1973 bool = true
                    ref_set__Ref_4bool(done__23, inline1973)
                    continue
                } else {
                    var t1033 string
                    var inline1984 string = ref_get__Ref_6string(text__21)
                    t1033 = inline1984
                    var t1034 string
                    var inline1982 string = char_to_string(ch__24)
                    t1034 = inline1982
                    var t1035 string = t1033 + t1034
                    ref_set__Ref_6string(text__21, t1035)
                    var t1036 int
                    var inline1978 int = ref_get__Ref_3int(i__22)
                    t1036 = inline1978
                    var t1037 int = t1036 + 1
                    ref_set__Ref_3int(i__22, t1037)
                    continue
                }
            } else {
                var inline1989 bool = ch__24 == 32
                t1031 = inline1989
                if t1031 {
                    var inline1973 bool = true
                    ref_set__Ref_4bool(done__23, inline1973)
                    continue
                } else {
                    var t1033 string
                    var inline1984 string = ref_get__Ref_6string(text__21)
                    t1033 = inline1984
                    var t1034 string
                    var inline1982 string = char_to_string(ch__24)
                    t1034 = inline1982
                    var t1035 string = t1033 + t1034
                    ref_set__Ref_6string(text__21, t1035)
                    var t1036 int
                    var inline1978 int = ref_get__Ref_3int(i__22)
                    t1036 = inline1978
                    var t1037 int = t1036 + 1
                    ref_set__Ref_3int(i__22, t1037)
                    continue
                }
            }
        } else {
            break Loop_loop1026
        }
    }
    var atom__25 string
    var inline2000 string = ref_get__Ref_6string(text__21)
    atom__25 = inline2000
    var jp1015 Token
    switch atom__25 {
    case "true":
        var t1018 Token = Token_Bool{
            _0: true,
        }
        jp1015 = t1018
    case "false":
        var t1019 Token = Token_Bool{
            _0: false,
        }
        jp1015 = t1019
    default:
        var t1022 bool = is_int_text(atom__25)
        if t1022 {
            var t1023 int32 = parse_int32(atom__25)
            var t1024 Token = Token_Int{
                _0: t1023,
            }
            jp1015 = t1024
        } else {
            var t1025 Token = Token_Sym{
                _0: atom__25,
            }
            jp1015 = t1025
        }
    }
    var t1016 int
    var inline1998 int = ref_get__Ref_3int(i__22)
    t1016 = inline1998
    var t1017 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp1015,
        _1: t1016,
    }
    return t1017
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline2055 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline2055
    var toks0__29 *_goml_vec_Token
    var inline2053 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline2053
    var toks__30 *ref_Vec_5Token_x
    var inline2051 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline2051
    var i__31 *ref_int_x
    var inline2048 int = 0
    var inline2049 *ref_int_x = ref__Ref_3int(inline2048)
    i__31 = inline2049
    Loop_loop1047:
    for {
        var t1048 int
        var inline2044 int = ref_get__Ref_3int(i__31)
        t1048 = inline2044
        var t1049 bool = t1048 < len__28
        if t1049 {
            var t1050 int
            var inline2042 int = ref_get__Ref_3int(i__31)
            t1050 = inline2042
            var ch__32 rune
            var inline2040 rune = string_get(source__27, t1050)
            ch__32 = inline2040
            var t1052 bool = ch__32 == 40
            if t1052 {
                var t1053 *_goml_vec_Token
                var inline2018 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t1053 = inline2018
                var t1054 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t1053, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t1054)
                var t1055 int
                var inline2014 int = ref_get__Ref_3int(i__31)
                t1055 = inline2014
                var t1056 int = t1055 + 1
                ref_set__Ref_3int(i__31, t1056)
                continue
            } else {
                var t1059 bool = ch__32 == 41
                if t1059 {
                    var t1060 *_goml_vec_Token
                    var inline2026 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t1060 = inline2026
                    var t1061 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t1060, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t1061)
                    var t1062 int
                    var inline2022 int = ref_get__Ref_3int(i__31)
                    t1062 = inline2022
                    var t1063 int = t1062 + 1
                    ref_set__Ref_3int(i__31, t1063)
                    continue
                } else {
                    var t1066 bool = ch__32 == 32
                    if t1066 {
                        var t1067 int
                        var inline2030 int = ref_get__Ref_3int(i__31)
                        t1067 = inline2030
                        var t1068 int = t1067 + 1
                        ref_set__Ref_3int(i__31, t1068)
                        continue
                    } else {
                        var t1070 int
                        var inline2038 int = ref_get__Ref_3int(i__31)
                        t1070 = inline2038
                        var mtmp809 Tuple2_5Token_3int = lex_atom(source__27, t1070)
                        var x810 Token = mtmp809._0
                        var x811 int = mtmp809._1
                        var t1071 *_goml_vec_Token
                        var inline2036 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t1071 = inline2036
                        var t1072 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t1071, x810)
                        ref_set__Ref_10Vec_5Token(toks__30, t1072)
                        ref_set__Ref_3int(i__31, x811)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop1047
        }
    }
    var inline2046 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline2046
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t1076 int
    var inline2081 int = vec_len__Vec_7Binding(env__35)
    t1076 = inline2081
    var t1077 int = t1076 - 1
    var i__37 *ref_int_x
    var inline2079 *ref_int_x = ref__Ref_3int(t1077)
    i__37 = inline2079
    var result__38 *ref_Value_x
    var inline2077 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline2077
    var done__39 *ref_bool_x
    var inline2074 bool = false
    var inline2075 *ref_bool_x = ref__Ref_4bool(inline2074)
    done__39 = inline2075
    Loop_loop1080:
    for {
        var t1092 bool
        var inline2070 bool = ref_get__Ref_4bool(done__39)
        t1092 = inline2070
        var t1093 bool = !t1092
        var jp1082 bool
        if t1093 {
            var t1094 int
            var inline2057 int = ref_get__Ref_3int(i__37)
            t1094 = inline2057
            var t1095 bool = t1094 >= 0
            jp1082 = t1095
        } else {
            jp1082 = false
        }
        if jp1082 {
            var t1083 int
            var inline2068 int = ref_get__Ref_3int(i__37)
            t1083 = inline2068
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t1083)
            var t1085 string = binding__40.name
            var t1086 bool = t1085 == name__36
            if t1086 {
                var t1087 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t1087)
                var inline2059 bool = true
                ref_set__Ref_4bool(done__39, inline2059)
                continue
            } else {
                var t1089 int
                var inline2066 int = ref_get__Ref_3int(i__37)
                t1089 = inline2066
                var t1090 int = t1089 - 1
                ref_set__Ref_3int(i__37, t1090)
                continue
            }
        } else {
            break Loop_loop1080
        }
    }
    var inline2072 Value = ref_get__Ref_5Value(result__38)
    return inline2072
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline2117 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline2117
    var exprs__48 *ref_Vec_5SExpr_x
    var inline2115 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline2115
    var i__49 *ref_int_x
    var inline2113 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline2113
    var done__50 *ref_bool_x
    var inline2110 bool = false
    var inline2111 *ref_bool_x = ref__Ref_4bool(inline2110)
    done__50 = inline2111
    Loop_loop1107:
    for {
        var t1119 bool
        var inline2104 bool = ref_get__Ref_4bool(done__50)
        t1119 = inline2104
        var t1120 bool = !t1119
        var jp1109 bool
        if t1120 {
            var t1121 int
            var inline2085 int = ref_get__Ref_3int(i__49)
            t1121 = inline2085
            var t1122 int
            var inline2083 int = vec_len__Vec_5Token(tokens__45)
            t1122 = inline2083
            var t1123 bool = t1121 < t1122
            jp1109 = t1123
        } else {
            jp1109 = false
        }
        if jp1109 {
            var t1110 int
            var inline2102 int = ref_get__Ref_3int(i__49)
            t1110 = inline2102
            var mtmp820 Token = vec_get__Vec_5Token(tokens__45, t1110)
            switch mtmp820.(type) {
            case RParen:
                var inline2091 bool = true
                ref_set__Ref_4bool(done__50, inline2091)
                var t1112 int
                var inline2089 int = ref_get__Ref_3int(i__49)
                t1112 = inline2089
                var t1113 int = t1112 + 1
                ref_set__Ref_3int(i__49, t1113)
                continue
            default:
                var t1115 int
                var inline2100 int = ref_get__Ref_3int(i__49)
                t1115 = inline2100
                var mtmp825 Tuple2_5SExpr_3int = parse_expr(tokens__45, t1115)
                var x826 SExpr = mtmp825._0
                var x827 int = mtmp825._1
                var t1116 *_goml_vec_SExpr
                var inline2098 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t1116 = inline2098
                var t1117 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t1116, x826)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t1117)
                ref_set__Ref_3int(i__49, x827)
                continue
            }
        } else {
            break Loop_loop1107
        }
    }
    var t1104 *_goml_vec_SExpr
    var inline2108 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t1104 = inline2108
    var t1105 int
    var inline2106 int = ref_get__Ref_3int(i__49)
    t1105 = inline2106
    var t1106 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t1104,
        _1: t1105,
    }
    return t1106
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp830 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp830.(type) {
    case LParen:
        var t1128 int = start__54 + 1
        var mtmp834 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t1128)
        var x835 *_goml_vec_SExpr = mtmp834._0
        var x836 int = mtmp834._1
        var t1129 SExpr = List{
            _0: x835,
        }
        var t1130 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t1129,
            _1: x836,
        }
        return t1130
    case RParen:
        var t1131 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t1132 int = start__54 + 1
        var t1133 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t1131,
            _1: t1132,
        }
        return t1133
    case Token_Sym:
        var x831 string = mtmp830.(Token_Sym)._0
        var t1134 SExpr = SExpr_Sym{
            _0: x831,
        }
        var t1135 int = start__54 + 1
        var t1136 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t1134,
            _1: t1135,
        }
        return t1136
    case Token_Int:
        var x832 int32 = mtmp830.(Token_Int)._0
        var t1137 SExpr = SExpr_Int{
            _0: x832,
        }
        var t1138 int = start__54 + 1
        var t1139 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t1137,
            _1: t1138,
        }
        return t1139
    case Token_Bool:
        var x833 bool = mtmp830.(Token_Bool)._0
        var t1140 SExpr = SExpr_Bool{
            _0: x833,
        }
        var t1141 int = start__54 + 1
        var t1142 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t1140,
            _1: t1141,
        }
        return t1142
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline2137 int = 0
    var inline2138 *ref_int_x = ref__Ref_3int(inline2137)
    i__61 = inline2138
    var acc__62 *_goml_vec_SExpr
    var inline2135 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline2135
    var exprs__63 *ref_Vec_5SExpr_x
    var inline2133 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline2133
    Loop_loop1147:
    for {
        var t1148 int
        var inline2129 int = ref_get__Ref_3int(i__61)
        t1148 = inline2129
        var t1149 int
        var inline2127 int = vec_len__Vec_5Token(tokens__60)
        t1149 = inline2127
        var t1150 bool = t1148 < t1149
        if t1150 {
            var t1151 int
            var inline2125 int = ref_get__Ref_3int(i__61)
            t1151 = inline2125
            var mtmp837 Tuple2_5SExpr_3int = parse_expr(tokens__60, t1151)
            var x838 SExpr = mtmp837._0
            var x839 int = mtmp837._1
            var t1152 *_goml_vec_SExpr
            var inline2123 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t1152 = inline2123
            var t1153 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t1152, x838)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t1153)
            ref_set__Ref_3int(i__61, x839)
            continue
        } else {
            break Loop_loop1147
        }
    }
    var inline2131 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline2131
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x848 int32 = expr__72.(SExpr_Int)._0
        var t1170 Value = Value_Int{
            _0: x848,
        }
        return t1170
    case SExpr_Bool:
        var x849 bool = expr__72.(SExpr_Bool)._0
        var t1171 Value = Value_Bool{
            _0: x849,
        }
        return t1171
    case SExpr_Sym:
        var x850 string = expr__72.(SExpr_Sym)._0
        var t1172 *_goml_vec_Binding
        var inline2148 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t1172 = inline2148
        var inline2144 Value = env_lookup(local__73, x850)
        switch inline2144.(type) {
        case Nil:
            var inline2145 Value = env_lookup(t1172, x850)
            return inline2145
        default:
            return inline2144
        }
    case List:
        var x851 *_goml_vec_SExpr = expr__72.(List)._0
        var inline2150 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x851)
        var inline2151 bool = inline2150 == 0
        if inline2151 {
            return Nil{}
        } else {
            var inline2152 SExpr = vec_get__Vec_5SExpr(x851, 0)
            switch inline2152.(type) {
            case SExpr_Sym:
                var inline2153 string = inline2152.(SExpr_Sym)._0
                var inline2155 Value = eval_list_sym(inline2153, x851, local__73, global__74)
                return inline2155
            default:
                var inline2156 Value = eval(inline2152, local__73, global__74)
                var inline2157 *_goml_vec_Value = eval_args(x851, 1, local__73, global__74)
                var inline2158 Value = apply(inline2156, inline2157, global__74)
                return inline2158
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t1189 Value = eval_begin(items__87, 1, local__88, global__89)
        return t1189
    case "define":
        var t1192 int
        var inline2170 int = vec_len__Vec_5SExpr(items__87)
        t1192 = inline2170
        var t1193 bool = t1192 == 3
        if t1193 {
            var mtmp856 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp856.(type) {
            case SExpr_Sym:
                var x859 string = mtmp856.(SExpr_Sym)._0
                var t1196 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t1196, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline2168 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline2168
                var t1197 Binding = Binding{
                    name: x859,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t1197)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t1200 int
        var inline2178 int = vec_len__Vec_5SExpr(items__87)
        t1200 = inline2178
        var t1201 bool = t1200 == 4
        if t1201 {
            var t1202 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t1202, local__88, global__89)
            var t1205 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline2172 int32 = cond__94.(Value_Int)._0
                var inline2174 bool = inline2172 != 0
                t1205 = inline2174
            case Value_Bool:
                var inline2175 bool = cond__94.(Value_Bool)._0
                t1205 = inline2175
            case Func:
                t1205 = true
            case Nil:
                t1205 = false
            default:
                panic("non-exhaustive match")
            }
            if t1205 {
                var t1206 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t1207 Value = eval(t1206, local__88, global__89)
                return t1207
            } else {
                var t1208 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t1209 Value = eval(t1208, local__88, global__89)
                return t1209
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t1212 int
        var inline2180 int = vec_len__Vec_5SExpr(items__87)
        t1212 = inline2180
        var t1213 bool = t1212 == 3
        if t1213 {
            var mtmp862 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp862.(type) {
            case List:
                var x866 *_goml_vec_SExpr = mtmp862.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x866)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t1216 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t1217 Value = Func{
                    _0: t1216,
                }
                return t1217
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t1218 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t1219 Value = apply_builtin("+", t1218)
        return t1219
    case "-":
        var t1220 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t1221 Value = apply_builtin("-", t1220)
        return t1221
    case "*":
        var t1222 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t1223 Value = apply_builtin("*", t1222)
        return t1223
    case "/":
        var t1224 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t1225 Value = apply_builtin("/", t1224)
        return t1225
    case "=":
        var t1226 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t1227 Value = apply_builtin("=", t1226)
        return t1227
    default:
        var t1228 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t1228, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline2182 Lambda = f__98.(Func)._0
            var inline2184 Value = apply_lambda(inline2182, args__99)
            return inline2184
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline2202 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline2202
    var last__105 *ref_Value_x
    var inline2200 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline2200
    Loop_loop1234:
    for {
        var t1235 int
        var inline2196 int = ref_get__Ref_3int(i__104)
        t1235 = inline2196
        var t1236 int
        var inline2194 int = vec_len__Vec_5SExpr(items__100)
        t1236 = inline2194
        var t1237 bool = t1235 < t1236
        if t1237 {
            var t1238 int
            var inline2192 int = ref_get__Ref_3int(i__104)
            t1238 = inline2192
            var t1239 SExpr = vec_get__Vec_5SExpr(items__100, t1238)
            var v__106 Value = eval(t1239, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t1240 int
            var inline2188 int = ref_get__Ref_3int(i__104)
            t1240 = inline2188
            var t1241 int = t1240 + 1
            ref_set__Ref_3int(i__104, t1241)
            continue
        } else {
            break Loop_loop1234
        }
    }
    var inline2198 Value = ref_get__Ref_5Value(last__105)
    return inline2198
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline2228 int = 0
    var inline2229 *ref_int_x = ref__Ref_3int(inline2228)
    i__108 = inline2229
    var acc__109 *_goml_vec_string
    var inline2226 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline2226
    var params__110 *ref_Vec_6string_x
    var inline2224 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline2224
    Loop_loop1247:
    for {
        var t1248 int
        var inline2220 int = ref_get__Ref_3int(i__108)
        t1248 = inline2220
        var t1249 int
        var inline2218 int = vec_len__Vec_5SExpr(items__107)
        t1249 = inline2218
        var t1250 bool = t1248 < t1249
        if t1250 {
            var t1251 int
            var inline2216 int = ref_get__Ref_3int(i__108)
            t1251 = inline2216
            var mtmp869 SExpr = vec_get__Vec_5SExpr(items__107, t1251)
            switch mtmp869.(type) {
            case SExpr_Sym:
                var x872 string = mtmp869.(SExpr_Sym)._0
                var t1253 *_goml_vec_string
                var inline2210 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t1253 = inline2210
                var t1254 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t1253, x872)
                ref_set__Ref_11Vec_6string(params__110, t1254)
                var t1255 int
                var inline2206 int = ref_get__Ref_3int(i__108)
                t1255 = inline2206
                var t1256 int = t1255 + 1
                ref_set__Ref_3int(i__108, t1256)
                continue
            default:
                var t1258 int
                var inline2214 int = ref_get__Ref_3int(i__108)
                t1258 = inline2214
                var t1259 int = t1258 + 1
                ref_set__Ref_3int(i__108, t1259)
                continue
            }
        } else {
            break Loop_loop1247
        }
    }
    var inline2222 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline2222
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline2251 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline2251
    var acc__117 *_goml_vec_Value
    var inline2249 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline2249
    var args__118 *ref_Vec_5Value_x
    var inline2247 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline2247
    Loop_loop1265:
    for {
        var t1266 int
        var inline2243 int = ref_get__Ref_3int(i__116)
        t1266 = inline2243
        var t1267 int
        var inline2241 int = vec_len__Vec_5SExpr(items__112)
        t1267 = inline2241
        var t1268 bool = t1266 < t1267
        if t1268 {
            var t1269 int
            var inline2239 int = ref_get__Ref_3int(i__116)
            t1269 = inline2239
            var t1270 SExpr = vec_get__Vec_5SExpr(items__112, t1269)
            var v__119 Value = eval(t1270, local__114, global__115)
            var t1271 *_goml_vec_Value
            var inline2237 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t1271 = inline2237
            var t1272 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t1271, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t1272)
            var t1273 int
            var inline2233 int = ref_get__Ref_3int(i__116)
            t1273 = inline2233
            var t1274 int = t1273 + 1
            ref_set__Ref_3int(i__116, t1274)
            continue
        } else {
            break Loop_loop1265
        }
    }
    var inline2245 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline2245
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t1282 int
        var inline2253 int = vec_len__Vec_5Value(args__121)
        t1282 = inline2253
        var t1283 bool = t1282 == 2
        if t1283 {
            var t1284 Value = vec_get__Vec_5Value(args__121, 0)
            var t1285 Value = vec_get__Vec_5Value(args__121, 1)
            switch t1285.(type) {
            case Value_Int:
                var x881 int32 = t1285.(Value_Int)._0
                switch t1284.(type) {
                case Value_Int:
                    var x884 int32 = t1284.(Value_Int)._0
                    var t1290 bool = x884 == x881
                    var t1291 Value = Value_Bool{
                        _0: t1290,
                    }
                    return t1291
                default:
                    var t1292 Value = Value_Bool{
                        _0: false,
                    }
                    return t1292
                }
            case Value_Bool:
                var x882 bool = t1285.(Value_Bool)._0
                switch t1284.(type) {
                case Value_Bool:
                    var x888 bool = t1284.(Value_Bool)._0
                    var t1295 bool = x888 == x882
                    var t1296 Value = Value_Bool{
                        _0: t1295,
                    }
                    return t1296
                default:
                    var t1297 Value = Value_Bool{
                        _0: false,
                    }
                    return t1297
                }
            default:
                var t1298 Value = Value_Bool{
                    _0: false,
                }
                return t1298
            }
        } else {
            var t1299 Value = Value_Bool{
                _0: false,
            }
            return t1299
        }
    case "+":
        var i__126 *ref_int_x
        var inline2278 int = 0
        var inline2279 *ref_int_x = ref__Ref_3int(inline2278)
        i__126 = inline2279
        var acc__127 *ref_int32_x
        var inline2275 int32 = 0
        var inline2276 *ref_int32_x = ref__Ref_5int32(inline2275)
        acc__127 = inline2276
        Loop_loop1303:
        for {
            var t1304 int
            var inline2271 int = ref_get__Ref_3int(i__126)
            t1304 = inline2271
            var t1305 int
            var inline2269 int = vec_len__Vec_5Value(args__121)
            t1305 = inline2269
            var t1306 bool = t1304 < t1305
            if t1306 {
                var t1307 int
                var inline2267 int = ref_get__Ref_3int(i__126)
                t1307 = inline2267
                var mtmp890 Value = vec_get__Vec_5Value(args__121, t1307)
                switch mtmp890.(type) {
                case Value_Int:
                    var x891 int32 = mtmp890.(Value_Int)._0
                    var t1309 int32
                    var inline2261 int32 = ref_get__Ref_5int32(acc__127)
                    t1309 = inline2261
                    var t1310 int32 = t1309 + x891
                    ref_set__Ref_5int32(acc__127, t1310)
                    var t1311 int
                    var inline2257 int = ref_get__Ref_3int(i__126)
                    t1311 = inline2257
                    var t1312 int = t1311 + 1
                    ref_set__Ref_3int(i__126, t1312)
                    continue
                default:
                    var t1314 int
                    var inline2265 int = ref_get__Ref_3int(i__126)
                    t1314 = inline2265
                    var t1315 int = t1314 + 1
                    ref_set__Ref_3int(i__126, t1315)
                    continue
                }
            } else {
                break Loop_loop1303
            }
        }
        var t1301 int32
        var inline2273 int32 = ref_get__Ref_5int32(acc__127)
        t1301 = inline2273
        var t1302 Value = Value_Int{
            _0: t1301,
        }
        return t1302
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
        var acc__130 *ref_int32_x
        var inline2301 int32 = 1
        var inline2302 *ref_int32_x = ref__Ref_5int32(inline2301)
        acc__130 = inline2302
        Loop_loop1320:
        for {
            var t1321 int
            var inline2297 int = ref_get__Ref_3int(i__129)
            t1321 = inline2297
            var t1322 int
            var inline2295 int = vec_len__Vec_5Value(args__121)
            t1322 = inline2295
            var t1323 bool = t1321 < t1322
            if t1323 {
                var t1324 int
                var inline2293 int = ref_get__Ref_3int(i__129)
                t1324 = inline2293
                var mtmp896 Value = vec_get__Vec_5Value(args__121, t1324)
                switch mtmp896.(type) {
                case Value_Int:
                    var x897 int32 = mtmp896.(Value_Int)._0
                    var t1326 int32
                    var inline2287 int32 = ref_get__Ref_5int32(acc__130)
                    t1326 = inline2287
                    var t1327 int32 = t1326 * x897
                    ref_set__Ref_5int32(acc__130, t1327)
                    var t1328 int
                    var inline2283 int = ref_get__Ref_3int(i__129)
                    t1328 = inline2283
                    var t1329 int = t1328 + 1
                    ref_set__Ref_3int(i__129, t1329)
                    continue
                default:
                    var t1331 int
                    var inline2291 int = ref_get__Ref_3int(i__129)
                    t1331 = inline2291
                    var t1332 int = t1331 + 1
                    ref_set__Ref_3int(i__129, t1332)
                    continue
                }
            } else {
                break Loop_loop1320
            }
        }
        var t1318 int32
        var inline2299 int32 = ref_get__Ref_5int32(acc__130)
        t1318 = inline2299
        var t1319 Value = Value_Int{
            _0: t1318,
        }
        return t1319
    case "-":
        var mtmp902 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp902 {
        case 1:
            var mtmp903 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp903.(type) {
            case Value_Int:
                var x904 int32 = mtmp903.(Value_Int)._0
                var t1338 int32 = 0 - x904
                var t1339 Value = Value_Int{
                    _0: t1338,
                }
                return t1339
            default:
                return Nil{}
            }
        case 2:
            var t1340 Value = vec_get__Vec_5Value(args__121, 0)
            var t1341 Value = vec_get__Vec_5Value(args__121, 1)
            switch t1341.(type) {
            case Value_Int:
                var x910 int32 = t1341.(Value_Int)._0
                switch t1340.(type) {
                case Value_Int:
                    var x913 int32 = t1340.(Value_Int)._0
                    var t1346 int32 = x913 - x910
                    var t1347 Value = Value_Int{
                        _0: t1346,
                    }
                    return t1347
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        default:
            return Nil{}
        }
    case "/":
        var t1350 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t1351 bool = t1350 == 2
        if t1351 {
            var t1352 Value = vec_get__Vec_5Value(args__121, 0)
            var t1353 Value = vec_get__Vec_5Value(args__121, 1)
            switch t1353.(type) {
            case Value_Int:
                var x919 int32 = t1353.(Value_Int)._0
                switch t1352.(type) {
                case Value_Int:
                    var x922 int32 = t1352.(Value_Int)._0
                    var t1358 int32 = x922 / x919
                    var t1359 Value = Value_Int{
                        _0: t1358,
                    }
                    return t1359
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    default:
        return Nil{}
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    switch func__137.(type) {
    case Func:
        var x927 Lambda = func__137.(Func)._0
        var t1364 Value = apply_lambda(x927, args__138)
        return t1364
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t1367 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline2329 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t1367)
    env__143 = inline2329
    var i__144 *ref_int_x
    var inline2326 int = 0
    var inline2327 *ref_int_x = ref__Ref_3int(inline2326)
    i__144 = inline2327
    Loop_loop1373:
    for {
        var t1384 int
        var inline2322 int = ref_get__Ref_3int(i__144)
        t1384 = inline2322
        var t1385 *_goml_vec_string = lambda__141.params
        var t1386 int
        var inline2320 int = vec_len__Vec_6string(t1385)
        t1386 = inline2320
        var t1387 bool = t1384 < t1386
        var jp1375 bool
        if t1387 {
            var t1388 int
            var inline2306 int = ref_get__Ref_3int(i__144)
            t1388 = inline2306
            var t1389 int
            var inline2304 int = vec_len__Vec_5Value(args__142)
            t1389 = inline2304
            var t1390 bool = t1388 < t1389
            jp1375 = t1390
        } else {
            jp1375 = false
        }
        if jp1375 {
            var t1376 *_goml_vec_string = lambda__141.params
            var t1377 int
            var inline2318 int = ref_get__Ref_3int(i__144)
            t1377 = inline2318
            var name__145 string = vec_get__Vec_6string(t1376, t1377)
            var t1378 int
            var inline2316 int = ref_get__Ref_3int(i__144)
            t1378 = inline2316
            var value__146 Value = vec_get__Vec_5Value(args__142, t1378)
            var t1379 *_goml_vec_Binding
            var inline2314 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t1379 = inline2314
            var t1380 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t1379, t1380)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t1381 int
            var inline2310 int = ref_get__Ref_3int(i__144)
            t1381 = inline2310
            var t1382 int = t1381 + 1
            ref_set__Ref_3int(i__144, t1382)
            continue
        } else {
            break Loop_loop1373
        }
    }
    var t1369 SExpr = lambda__141.body
    var t1370 *_goml_vec_Binding
    var inline2324 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t1370 = inline2324
    var t1371 *ref_Vec_7Binding_x = lambda__141.global
    var t1372 Value = eval(t1369, t1370, t1371)
    return t1372
}

func main0() struct{} {
    var t1392 *_goml_vec_Binding
    var inline2357 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1392 = inline2357
    var global__148 *ref_Vec_7Binding_x
    var inline2355 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t1392)
    global__148 = inline2355
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t1393 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t1393)
    var t1394 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t1395 *_goml_vec_Binding
    var inline2353 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1395 = inline2353
    var result__151 Value = eval(t1394, t1395, global__148)
    var t1396 string
    switch result__151.(type) {
    case Value_Int:
        var inline2346 int32 = result__151.(Value_Int)._0
        var inline2348 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline2346)
        t1396 = inline2348
    case Value_Bool:
        var inline2349 bool = result__151.(Value_Bool)._0
        var inline2351 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline2349)
        t1396 = inline2351
    case Func:
        t1396 = "<lambda>"
    case Nil:
        t1396 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline2343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1396)
    _goml_runtime_core_string_println(inline2343)
    var t1397 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t1397)
    var t1398 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t1399 *_goml_vec_Binding
    var inline2341 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1399 = inline2341
    var result2__153 Value = eval(t1398, t1399, global__148)
    var t1400 string
    switch result2__153.(type) {
    case Value_Int:
        var inline2334 int32 = result2__153.(Value_Int)._0
        var inline2336 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline2334)
        t1400 = inline2336
    case Value_Bool:
        var inline2337 bool = result2__153.(Value_Bool)._0
        var inline2339 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline2337)
        t1400 = inline2339
    case Func:
        t1400 = "<lambda>"
    case Nil:
        t1400 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline2331 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1400)
    _goml_runtime_core_string_println(inline2331)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__288 string) int {
    var t1403 int = _goml_runtime_core_string_len(self__288)
    return t1403
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t1406 *ref_int_x = ref__Ref_3int(value__684)
    return t1406
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__684 bool) *ref_bool_x {
    var t1409 *ref_bool_x = ref__Ref_4bool(value__684)
    return t1409
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__513 *_goml_vec_Token, elem__514 Token) *_goml_vec_Token {
    var t1453 int
    var inline2379 int = vec_len__Vec_5Token(self__513)
    t1453 = inline2379
    var t1454 int = t1453 + 1
    var result__515 *_goml_vec_Token
    var inline2377 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t1454)
    result__515 = inline2377
    var index__516 int = 0
    Loop_loop1456:
    for {
        var t1457 int
        var inline2373 int = vec_len__Vec_5Token(self__513)
        t1457 = inline2373
        var t1458 bool = index__516 < t1457
        if t1458 {
            var t1459 Token = vec_get__Vec_5Token(self__513, index__516)
            vec_push__Vec_5Token(result__515, t1459)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1460 int = compound_old575 + compound_value576
            index__516 = t1460
            continue
        } else {
            break Loop_loop1456
        }
    }
    vec_push__Vec_5Token(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__513 *_goml_vec_SExpr, elem__514 SExpr) *_goml_vec_SExpr {
    var t1489 int
    var inline2389 int = vec_len__Vec_5SExpr(self__513)
    t1489 = inline2389
    var t1490 int = t1489 + 1
    var result__515 *_goml_vec_SExpr
    var inline2387 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t1490)
    result__515 = inline2387
    var index__516 int = 0
    Loop_loop1492:
    for {
        var t1493 int
        var inline2383 int = vec_len__Vec_5SExpr(self__513)
        t1493 = inline2383
        var t1494 bool = index__516 < t1493
        if t1494 {
            var t1495 SExpr = vec_get__Vec_5SExpr(self__513, index__516)
            vec_push__Vec_5SExpr(result__515, t1495)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1496 int = compound_old575 + compound_value576
            index__516 = t1496
            continue
        } else {
            break Loop_loop1492
        }
    }
    vec_push__Vec_5SExpr(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline2391 int64 = int64(int32(self__286))
    var inline2392 string = signed_decimal_string(inline2391)
    return inline2392
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1505 string = _goml_runtime_core_bool_to_string(self__401)
    return t1505
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__526 *_goml_vec_SExpr) int {
    var t1511 int = vec_len__Vec_5SExpr(self__526)
    return t1511
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__513 *_goml_vec_Binding, elem__514 Binding) *_goml_vec_Binding {
    var t1514 int
    var inline2402 int = vec_len__Vec_7Binding(self__513)
    t1514 = inline2402
    var t1515 int = t1514 + 1
    var result__515 *_goml_vec_Binding
    var inline2400 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t1515)
    result__515 = inline2400
    var index__516 int = 0
    Loop_loop1517:
    for {
        var t1518 int
        var inline2396 int = vec_len__Vec_7Binding(self__513)
        t1518 = inline2396
        var t1519 bool = index__516 < t1518
        if t1519 {
            var t1520 Binding = vec_get__Vec_7Binding(self__513, index__516)
            vec_push__Vec_7Binding(result__515, t1520)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1521 int = compound_old575 + compound_value576
            index__516 = t1521
            continue
        } else {
            break Loop_loop1517
        }
    }
    vec_push__Vec_7Binding(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__513 *_goml_vec_string, elem__514 string) *_goml_vec_string {
    var t1536 int
    var inline2412 int = vec_len__Vec_6string(self__513)
    t1536 = inline2412
    var t1537 int = t1536 + 1
    var result__515 *_goml_vec_string
    var inline2410 *_goml_vec_string = vec_with_capacity__Vec_6string(t1537)
    result__515 = inline2410
    var index__516 int = 0
    Loop_loop1539:
    for {
        var t1540 int
        var inline2406 int = vec_len__Vec_6string(self__513)
        t1540 = inline2406
        var t1541 bool = index__516 < t1540
        if t1541 {
            var t1542 string = vec_get__Vec_6string(self__513, index__516)
            vec_push__Vec_6string(result__515, t1542)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1543 int = compound_old575 + compound_value576
            index__516 = t1543
            continue
        } else {
            break Loop_loop1539
        }
    }
    vec_push__Vec_6string(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__513 *_goml_vec_Value, elem__514 Value) *_goml_vec_Value {
    var t1558 int
    var inline2422 int = vec_len__Vec_5Value(self__513)
    t1558 = inline2422
    var t1559 int = t1558 + 1
    var result__515 *_goml_vec_Value
    var inline2420 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t1559)
    result__515 = inline2420
    var index__516 int = 0
    Loop_loop1561:
    for {
        var t1562 int
        var inline2416 int = vec_len__Vec_5Value(self__513)
        t1562 = inline2416
        var t1563 bool = index__516 < t1562
        if t1563 {
            var t1564 Value = vec_get__Vec_5Value(self__513, index__516)
            vec_push__Vec_5Value(result__515, t1564)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1565 int = compound_old575 + compound_value576
            index__516 = t1565
            continue
        } else {
            break Loop_loop1561
        }
    }
    vec_push__Vec_5Value(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__526 *_goml_vec_Value) int {
    var t1571 int = vec_len__Vec_5Value(self__526)
    return t1571
}

func string_get(value__270 string, index__271 int) rune {
    var mtmp391 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__270, index__271)
    var x392 bool = mtmp391._0
    var x393 rune = mtmp391._1
    if x392 {
        return x393
    } else {
        var t1588 rune = _goml_runtime_core_string_get("", -1)
        return t1588
    }
}

func char_to_string(value__282 rune) string {
    var t1593 uint32 = uint32(rune(value__282))
    var t1594 bool
    var inline2425 bool = t1593 <= 1114111
    if inline2425 {
        var inline2426 bool = t1593 >= 55296
        var inline2428 bool
        if inline2426 {
            var inline2430 bool = t1593 <= 57343
            inline2428 = inline2430
        } else {
            inline2428 = false
        }
        var inline2429 bool = !inline2428
        t1594 = inline2429
    } else {
        t1594 = false
    }
    if t1594 {
        var t1595 string = _goml_runtime_core_char_to_string(value__282)
        return t1595
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1745 bool = index__259 < 0
    var jp1743 bool
    if t1745 {
        jp1743 = true
    } else {
        var t1746 bool = index__259 >= length__260
        jp1743 = t1746
    }
    if jp1743 {
        var inline2440 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2440
    } else {
        var t1630 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1630))
        var t1633 bool = first__261 < 128
        if t1633 {
            var inline2442 int = 1
            var inline2443 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline2443._tag {
            case 0:
                var inline2444 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2444
            case 1:
                var inline2445 rune = inline2443._v1_0
                var inline2447 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2445,
                    _2: inline2442,
                }
                return inline2447
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1637 bool = first__261 < 194
            if t1637 {
                var inline2449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2449
            } else {
                var t1641 bool = first__261 < 224
                if t1641 {
                    var t1654 int = length__260 - index__259
                    var t1655 bool = t1654 < 2
                    if t1655 {
                        var inline2451 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2451
                    } else {
                        var t1643 int = index__259 + 1
                        var t1644 uint8
                        var inline2465 uint8 = _goml_runtime_core_string_byte_get(value__258, t1643)
                        t1644 = inline2465
                        var second__262 uint32 = uint32(uint8(t1644))
                        var t1647 bool
                        var inline2462 bool = second__262 < 128
                        if inline2462 {
                            t1647 = true
                        } else {
                            var inline2463 bool = second__262 > 191
                            t1647 = inline2463
                        }
                        if t1647 {
                            var inline2453 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2453
                        } else {
                            var t1649_rhs uint32 = 31
                            var t1649 uint32 = first__261 & t1649_rhs
                            var t1650_rhs int = 6
                            var t1650 uint32 = t1649 << t1650_rhs
                            var t1651_rhs uint32 = 63
                            var t1651 uint32 = second__262 & t1651_rhs
                            var t1652 uint32 = t1650 | t1651
                            var inline2455 int = 2
                            var inline2456 Option__char = __goml_builtin_char_from_uint32(t1652)
                            switch inline2456._tag {
                            case 0:
                                var inline2457 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2457
                            case 1:
                                var inline2458 rune = inline2456._v1_0
                                var inline2460 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2458,
                                    _2: inline2455,
                                }
                                return inline2460
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1659 bool = first__261 < 240
                    if t1659 {
                        var t1692 int = length__260 - index__259
                        var t1693 bool = t1692 < 3
                        if t1693 {
                            var inline2467 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2467
                        } else {
                            var t1661 int = index__259 + 1
                            var t1662 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1661)
                            var second__263 uint32 = uint32(uint8(t1662))
                            var t1663 int = index__259 + 2
                            var t1664 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1663)
                            var third__264 uint32 = uint32(uint8(t1664))
                            var t1690 bool = utf8_invalid_continuation(second__263)
                            var jp1685 bool
                            if t1690 {
                                jp1685 = true
                            } else {
                                var inline2469 bool = third__264 < 128
                                if inline2469 {
                                    jp1685 = true
                                } else {
                                    var inline2470 bool = third__264 > 191
                                    jp1685 = inline2470
                                }
                            }
                            var jp1679 bool
                            if jp1685 {
                                jp1679 = true
                            } else {
                                var t1688 bool = first__261 == 224
                                if t1688 {
                                    var t1689 bool = second__263 < 160
                                    jp1679 = t1689
                                } else {
                                    jp1679 = false
                                }
                            }
                            var jp1668 bool
                            if jp1679 {
                                jp1668 = true
                            } else {
                                var t1682 bool = first__261 == 237
                                if t1682 {
                                    var t1683 bool = second__263 >= 160
                                    jp1668 = t1683
                                } else {
                                    jp1668 = false
                                }
                            }
                            if jp1668 {
                                var inline2472 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2472
                            } else {
                                var t1670_rhs uint32 = 15
                                var t1670 uint32 = first__261 & t1670_rhs
                                var t1671_rhs int = 12
                                var t1671 uint32 = t1670 << t1671_rhs
                                var t1672_rhs uint32 = 63
                                var t1672 uint32 = second__263 & t1672_rhs
                                var t1673_rhs int = 6
                                var t1673 uint32 = t1672 << t1673_rhs
                                var t1674 uint32 = t1671 | t1673
                                var t1675_rhs uint32 = 63
                                var t1675 uint32 = third__264 & t1675_rhs
                                var t1676 uint32 = t1674 | t1675
                                var inline2474 int = 3
                                var inline2475 Option__char = __goml_builtin_char_from_uint32(t1676)
                                switch inline2475._tag {
                                case 0:
                                    var inline2476 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2476
                                case 1:
                                    var inline2477 rune = inline2475._v1_0
                                    var inline2479 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2477,
                                        _2: inline2474,
                                    }
                                    return inline2479
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1697 bool = first__261 < 245
                        if t1697 {
                            var t1738 int = length__260 - index__259
                            var t1739 bool = t1738 < 4
                            if t1739 {
                                var t1740 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1740
                            } else {
                                var t1699 int = index__259 + 1
                                var t1700 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1699)
                                var second__265 uint32 = uint32(uint8(t1700))
                                var t1701 int = index__259 + 2
                                var t1702 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1701)
                                var third__266 uint32 = uint32(uint8(t1702))
                                var t1703 int = index__259 + 3
                                var t1704 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1703)
                                var fourth__267 uint32 = uint32(uint8(t1704))
                                var t1736 bool = utf8_invalid_continuation(second__265)
                                var jp1734 bool
                                if t1736 {
                                    jp1734 = true
                                } else {
                                    var t1737 bool = utf8_invalid_continuation(third__266)
                                    jp1734 = t1737
                                }
                                var jp1728 bool
                                if jp1734 {
                                    jp1728 = true
                                } else {
                                    var t1735 bool = utf8_invalid_continuation(fourth__267)
                                    jp1728 = t1735
                                }
                                var jp1722 bool
                                if jp1728 {
                                    jp1722 = true
                                } else {
                                    var t1731 bool = first__261 == 240
                                    if t1731 {
                                        var t1732 bool = second__265 < 144
                                        jp1722 = t1732
                                    } else {
                                        jp1722 = false
                                    }
                                }
                                var jp1708 bool
                                if jp1722 {
                                    jp1708 = true
                                } else {
                                    var t1725 bool = first__261 == 244
                                    if t1725 {
                                        var t1726 bool = second__265 > 143
                                        jp1708 = t1726
                                    } else {
                                        jp1708 = false
                                    }
                                }
                                if jp1708 {
                                    var t1709 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1709
                                } else {
                                    var t1710_rhs uint32 = 7
                                    var t1710 uint32 = first__261 & t1710_rhs
                                    var t1711_rhs int = 18
                                    var t1711 uint32 = t1710 << t1711_rhs
                                    var t1712_rhs uint32 = 63
                                    var t1712 uint32 = second__265 & t1712_rhs
                                    var t1713_rhs int = 12
                                    var t1713 uint32 = t1712 << t1713_rhs
                                    var t1714 uint32 = t1711 | t1713
                                    var t1715_rhs uint32 = 63
                                    var t1715 uint32 = third__266 & t1715_rhs
                                    var t1716_rhs int = 6
                                    var t1716 uint32 = t1715 << t1716_rhs
                                    var t1717 uint32 = t1714 | t1716
                                    var t1718_rhs uint32 = 63
                                    var t1718 uint32 = fourth__267 & t1718_rhs
                                    var t1719 uint32 = t1717 | t1718
                                    var t1720 Tuple3_4bool_4char_3int = utf8_valid_decode(t1719, 4)
                                    return t1720
                                }
                            }
                        } else {
                            var t1741 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1741
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1751 bool = value__257 <= 1114111
    if t1751 {
        var t1755 bool = value__257 >= 55296
        var jp1753 bool
        if t1755 {
            var t1756 bool = value__257 <= 57343
            jp1753 = t1756
        } else {
            jp1753 = false
        }
        var t1754 bool = !jp1753
        return t1754
    } else {
        return false
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1761 bool = value__214 < 0
    if t1761 {
        var t1762 uint64 = uint64(int64(value__214))
        var t1763 uint64 = 0 - t1762
        var t1764 string = decimal_string(t1763)
        var t1765 string = "-" + t1764
        return t1765
    } else {
        var t1766 uint64 = uint64(int64(value__214))
        var t1767 string = decimal_string(t1766)
        return t1767
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1770 int = _goml_runtime_core_string_len(self__289)
    return t1770
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1773 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1773
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1776 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1776
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2518 rune
    var inline2483 bool = utf8_valid_scalar(value__253)
    if inline2483 {
        var inline2484 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2485 rune = inline2484._1
        commute_field2518 = inline2485
        var t1782 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2518,
            _2: width__254,
        }
        return t1782
    } else {
        var inline2481 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2481
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1787 bool = value__256 < 128
    if t1787 {
        return true
    } else {
        var t1788 bool = value__256 > 191
        return t1788
    }
}

func decimal_string(value__208 uint64) string {
    var t1811 bool = value__208 == 0
    if t1811 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1804:
        for {
            var t1805 bool = remaining__210 > 0
            if t1805 {
                var t1806_rhs uint64 = 10
                var t1806 uint64 = remaining__210 % t1806_rhs
                var t1807 uint8 = uint8(uint64(t1806))
                var t1808 uint8 = t1807 + 48
                vec_push__Vec_5uint8(reversed__209, t1808)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1809 uint64 = compound_old353 / compound_value354
                remaining__210 = t1809
                continue
            } else {
                break Loop_loop1804
            }
        }
        var t1793 int
        var inline2497 int = vec_len__Vec_5uint8(reversed__209)
        t1793 = inline2497
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1793)
        var offset__212 int = 0
        Loop_loop1795:
        for {
            var t1796 int
            var inline2495 int = vec_len__Vec_5uint8(reversed__209)
            t1796 = inline2495
            var t1797 bool = offset__212 < t1796
            if t1797 {
                var t1798 int
                var inline2493 int = vec_len__Vec_5uint8(reversed__209)
                t1798 = inline2493
                var t1799 int = t1798 - offset__212
                var t1800 int = t1799 - 1
                var t1801 uint8 = vec_get__Vec_5uint8(reversed__209, t1800)
                vec_push__Vec_5uint8(bytes__211, t1801)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1802 int = compound_old358 + compound_value359
                offset__212 = t1802
                continue
            } else {
                break Loop_loop1795
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1816 bool
    var inline2499 bool = value__283 <= 1114111
    if inline2499 {
        var inline2500 bool = value__283 >= 55296
        var inline2502 bool
        if inline2500 {
            var inline2504 bool = value__283 <= 57343
            inline2502 = inline2504
        } else {
            inline2502 = false
        }
        var inline2503 bool = !inline2502
        t1816 = inline2503
    } else {
        t1816 = false
    }
    if t1816 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1817 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1817
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
