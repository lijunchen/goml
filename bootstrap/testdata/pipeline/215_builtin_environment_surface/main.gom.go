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
    var t74 string = _goml_runtime_core_int_to_string(native__0)
    var t75 string = _goml_runtime_core_int8_to_string(small__1)
    var t76 string = t74 + t75
    var t77 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t78 string = t76 + t77
    var t79 string = _goml_runtime_core_string_byte_slice("abcd", 1, 3)
    var text__3 string = t78 + t79
    var value__4 *ref_int32_x = ref__Ref_5int32(1)
    ref_set__Ref_5int32(value__4, 2)
    var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
    ptr_hash__Ref_5int32(value__4)
    var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t80 int32 = ref_get__Ref_5int32(value__4)
    vec_push__Vec_5int32(values__6, t80)
    vec_push__Vec_5int32(values__6, 3)
    vec_set__Vec_5int32(values__6, 1, 4)
    var t81 int = vec_len__Vec_5int32(values__6)
    var values_slice__7 []int32 = values__6.items[0:t81]
    var t82 int = len(values_slice__7)
    var nested__8 []int32 = values_slice__7[0:t82]
    var channel__9 chan int32 = func(p0 int) chan int32 {
        return make(chan int32, p0)
    }(1)
    var t83 int32 = nested__8[1]
    func(p0 chan int32, p1 int32) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__9, t83)
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
    var t84 FnIterator__int = _goml_m_range(0, 3)
    var t85 closure_env_main_0 = closure_env_main_0{}
    var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t84, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t85, p0, p1)
    })
    var t86 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(same__5)
    var t87 string = text__3 + t86
    var t88 int32 = received__10._0
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    var t90 string = t87 + t89
    var t91 bool = received__10._1
    var t92 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t91)
    var t93 string = t90 + t92
    var t94 string = _goml_m_inherent_i_int_i_int_i_to__string(total__13)
    var t95 string = t93 + t94
    println__T_string(t95)
    return struct{}{}
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv98 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    var running__121 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop101:
    for {
        var t102 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__121)
        if t102 {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__121, false)
            case Some:
                var x27 int = mtmp26.(Some)._0
                var value__122 int = x27
                var t105 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t106 int = combine__119(t105, value__122)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t106)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop101
        }
    }
    var t100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv98 = t100
    return retv98
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv109 FnIterator__int
    var t110 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv109 = t110
    return retv109
}

func println__T_string(value__1 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv115 string
    var t116 string = _goml_runtime_core_bool_to_string(self__37)
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__6)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int_to_string(self__5)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv124 *ref_int_x
    var t125 *ref_int_x = ref__Ref_3int(value__209)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv127 *ref_bool_x
    var t128 *ref_bool_x = ref__Ref_4bool(value__209)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv130 bool
    var t131 bool = ref_get__Ref_4bool(self__210)
    retv130 = t131
    return retv130
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv133 Option__int
    var t134 func() Option__int = self__102.next_fn
    var t135 Option__int = t134()
    retv133 = t135
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv139 int
    var t140 int = ref_get__Ref_3int(self__210)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv144 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t145 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__222,
        end_1: end__221,
    }
    var t146 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t145)
    })
    retv144 = t146
    return retv144
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv150 FnIterator__int
    var t151 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env71 closure_env_main_0, sum__11 int, item__12 int) int {
    var retv172 int
    var t173 int = sum__11 + item__12
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env72 closure_env_goml_builtin_range_1) Option__int {
    var retv175 Option__int
    var current__222 *ref_int_x = env72.current_0
    var end__221 int = env72.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t178 bool = value__223 < end__221
    var jp177 Option__int
    if t178 {
        var t179 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t179)
        var t180 Option__int = Some{
            _0: value__223,
        }
        jp177 = t180
    } else {
        jp177 = None{}
    }
    retv175 = jp177
    return retv175
}

func main() {
    main0()
}
