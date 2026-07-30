package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

func ptr_eq__Ref_5int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func ptr_hash__Ref_5int32(reference *ref_int32_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
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

type Tuple2_5int32_4bool struct {
    _0 int32
    _1 bool
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_main_0 struct {}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func main0() struct{} {
    var native__0 int = 7
    var small__1 int8 = 8
    var unsigned__2 uint8 = 9
    var t118 string = _goml_runtime_core_int_to_string(native__0)
    var t119 string = _goml_runtime_core_int8_to_string(small__1)
    var t120 string = t118 + t119
    var t121 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t122 string = t120 + t121
    var t123 string = _goml_runtime_core_string_byte_slice("abcd", 1, 3)
    var text__3 string = t122 + t123
    var value__4 *ref_int32_x = ref__Ref_5int32(1)
    ref_set__Ref_5int32(value__4, 2)
    var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
    ptr_hash__Ref_5int32(value__4)
    var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t124 int32 = ref_get__Ref_5int32(value__4)
    vec_push__Vec_5int32(values__6, t124)
    vec_push__Vec_5int32(values__6, 3)
    vec_set__Vec_5int32(values__6, 1, 4)
    var t125 int = vec_len__Vec_5int32(values__6)
    var values_slice__7 []int32 = values__6.items[0:t125]
    var t126 int = len(values_slice__7)
    var nested__8 []int32 = values_slice__7[0:t126]
    var channel__9 chan int32 = func(p0 int) chan int32 {
        return make(chan int32, p0)
    }(1)
    var t127 int32 = nested__8[1]
    func(p0 chan int32, p1 int32) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__9, t127)
    var received__10 Tuple2_5int32_4bool = func(p0 chan int32) Tuple2_5int32_4bool {
        var value int32
        var ok bool
        value, ok = <-p0
        return Tuple2_5int32_4bool{
            _0: value,
            _1: ok,
        }
    }(channel__9)
    func(p0 chan int32) struct{} {
        close(p0)
        return struct{}{}
    }(channel__9)
    var t128 FnIterator__int = _goml_m_range(0, 3)
    var t129 closure_env_main_0 = closure_env_main_0{}
    var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t128, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t129, p0, p1)
    })
    var t130 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(same__5)
    var t131 string = text__3 + t130
    var t132 int32 = received__10._0
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    var t134 string = t131 + t133
    var t135 bool = received__10._1
    var t136 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t135)
    var t137 string = t134 + t136
    var t138 string = _goml_m_inherent_i_int_i_int_i_to__string(total__13)
    var t139 string = t137 + t138
    println__T_string(t139)
    return struct{}{}
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv142 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    Loop_loop145:
    for {
        if true {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case None:
                break Loop_loop145
            case Some:
                var x27 int = mtmp26.(Some)._0
                var value__121 int = x27
                var t147 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t148 int = combine__119(t147, value__121)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t148)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop145
        }
    }
    var t144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv142 = t144
    return retv142
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv151 FnIterator__int
    var t152 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv151 = t152
    return retv151
}

func println__T_string(value__1 string) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv157 string
    var t158 string = _goml_runtime_core_bool_to_string(self__37)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv160 string
    var t161 string = _goml_runtime_core_int32_to_string(self__6)
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int_to_string(self__5)
    retv163 = t164
    return retv163
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv166 *ref_int_x
    var t167 *ref_int_x = ref__Ref_3int(value__207)
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv169 Option__int
    var t170 func() Option__int = self__102.next_fn
    var t171 Option__int = t170()
    retv169 = t171
    return retv169
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv173 int
    var t174 int = ref_get__Ref_3int(self__208)
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv178 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t179 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t180 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t179)
    })
    retv178 = t180
    return retv178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv182 string
    retv182 = self__38
    return retv182
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv184 FnIterator__int
    var t185 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env115 closure_env_main_0, sum__11 int, item__12 int) int {
    var retv199 int
    var t200 int = sum__11 + item__12
    retv199 = t200
    return retv199
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env116 closure_env_goml_builtin_range_1) Option__int {
    var retv202 Option__int
    var current__220 *ref_int_x = env116.current_0
    var end__219 int = env116.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t205 bool = value__221 < end__219
    var jp204 Option__int
    if t205 {
        var t206 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t206)
        var t207 Option__int = Some{
            _0: value__221,
        }
        jp204 = t207
    } else {
        jp204 = None{}
    }
    retv202 = jp204
    return retv202
}

func main() {
    main0()
}
