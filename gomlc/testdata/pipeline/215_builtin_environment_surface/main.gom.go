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
    var t78 string = _goml_runtime_core_int_to_string(native__0)
    var t79 string = _goml_runtime_core_int8_to_string(small__1)
    var t80 string = t78 + t79
    var t81 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t82 string = t80 + t81
    var t83 string = _goml_runtime_core_string_byte_slice("abcd", 1, 3)
    var text__3 string = t82 + t83
    var value__4 *ref_int32_x = ref__Ref_5int32(1)
    ref_set__Ref_5int32(value__4, 2)
    var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
    ptr_hash__Ref_5int32(value__4)
    var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t84 int32 = ref_get__Ref_5int32(value__4)
    vec_push__Vec_5int32(values__6, t84)
    vec_push__Vec_5int32(values__6, 3)
    vec_set__Vec_5int32(values__6, 1, 4)
    var t85 int = vec_len__Vec_5int32(values__6)
    var values_slice__7 []int32 = values__6.items[0:t85]
    var t86 int = len(values_slice__7)
    var nested__8 []int32 = values_slice__7[0:t86]
    var channel__9 chan int32 = func(p0 int) chan int32 {
        return make(chan int32, p0)
    }(1)
    var t87 int32 = nested__8[1]
    func(p0 chan int32, p1 int32) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__9, t87)
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
    var t88 FnIterator__int = _goml_m_range(0, 3)
    var t89 closure_env_main_0 = closure_env_main_0{}
    var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t88, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t89, p0, p1)
    })
    var t90 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(same__5)
    var t91 string = text__3 + t90
    var t92 int32 = received__10._0
    var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t92)
    var t94 string = t91 + t93
    var t95 bool = received__10._1
    var t96 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t95)
    var t97 string = t94 + t96
    var t98 string = _goml_m_inherent_i_int_i_int_i_to__string(total__13)
    var t99 string = t97 + t98
    println__T_string(t99)
    return struct{}{}
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv102 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    Loop_loop105:
    for {
        if true {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case None:
                break Loop_loop105
            case Some:
                var x27 int = mtmp26.(Some)._0
                var value__121 int = x27
                var t107 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t108 int = combine__119(t107, value__121)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t108)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop105
        }
    }
    var t104 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv102 = t104
    return retv102
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv111 FnIterator__int
    var t112 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv111 = t112
    return retv111
}

func println__T_string(value__1 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv117 string
    var t118 string = _goml_runtime_core_bool_to_string(self__37)
    retv117 = t118
    return retv117
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__6)
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int_to_string(self__5)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv126 *ref_int_x
    var t127 *ref_int_x = ref__Ref_3int(value__207)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv129 Option__int
    var t130 func() Option__int = self__102.next_fn
    var t131 Option__int = t130()
    retv129 = t131
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv133 int
    var t134 int = ref_get__Ref_3int(self__208)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv138 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t139 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t140 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t139)
    })
    retv138 = t140
    return retv138
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv142 string
    retv142 = self__38
    return retv142
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv144 FnIterator__int
    var t145 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv144 = t145
    return retv144
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env75 closure_env_main_0, sum__11 int, item__12 int) int {
    var retv159 int
    var t160 int = sum__11 + item__12
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env76 closure_env_goml_builtin_range_1) Option__int {
    var retv162 Option__int
    var current__220 *ref_int_x = env76.current_0
    var end__219 int = env76.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t165 bool = value__221 < end__219
    var jp164 Option__int
    if t165 {
        var t166 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t166)
        var t167 Option__int = Some{
            _0: value__221,
        }
        jp164 = t167
    } else {
        jp164 = None{}
    }
    retv162 = jp164
    return retv162
}

func main() {
    main0()
}
