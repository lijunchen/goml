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
    var t165 string = _goml_runtime_core_int_to_string(native__0)
    var t166 string = _goml_runtime_core_int8_to_string(small__1)
    var t167 string = t165 + t166
    var t168 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t169 string = t167 + t168
    var t170 string = _goml_runtime_core_string_byte_slice("abcd", 1, 3)
    var text__3 string = t169 + t170
    var value__4 *ref_int32_x = ref__Ref_5int32(1)
    ref_set__Ref_5int32(value__4, 2)
    var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
    ptr_hash__Ref_5int32(value__4)
    var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t171 int32 = ref_get__Ref_5int32(value__4)
    vec_push__Vec_5int32(values__6, t171)
    vec_push__Vec_5int32(values__6, 3)
    vec_set__Vec_5int32(values__6, 1, 4)
    var t172 int = vec_len__Vec_5int32(values__6)
    var values_slice__7 []int32 = values__6.items[0:t172]
    var t173 int = len(values_slice__7)
    var nested__8 []int32 = values_slice__7[0:t173]
    var channel__9 chan int32 = func(p0 int) chan int32 {
        return make(chan int32, p0)
    }(1)
    var t174 int32 = nested__8[1]
    func(p0 chan int32, p1 int32) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__9, t174)
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
    var t175 FnIterator__int
    var inline256 int = 0
    var inline257 int = 3
    var inline258 FnIterator__int = __goml_builtin_range(inline256, inline257)
    t175 = inline258
    var t176 closure_env_main_0 = closure_env_main_0{}
    var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t175, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t176, p0, p1)
    })
    var t177 string
    var inline254 string = _goml_runtime_core_bool_to_string(same__5)
    t177 = inline254
    var t178 string = text__3 + t177
    var t179 int32 = received__10._0
    var t180 string
    var inline252 string = _goml_runtime_core_int32_to_string(t179)
    t180 = inline252
    var t181 string = t178 + t180
    var t182 bool = received__10._1
    var t183 string
    var inline250 string = _goml_runtime_core_bool_to_string(t182)
    t183 = inline250
    var t184 string = t181 + t183
    var t185 string
    var inline248 string = _goml_runtime_core_int_to_string(total__13)
    t185 = inline248
    var t186 string = t184 + t185
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var accumulator__120 int = initial__118
    Loop_loop_expr191:
    for {
        var mtmp28 Option__int
        var inline260 func() Option__int = iterator__117.next_fn
        var inline261 Option__int = inline260()
        mtmp28 = inline261
        switch mtmp28.(type) {
        case None:
            break Loop_loop_expr191
        case Some:
            var x29 int = mtmp28.(Some)._0
            var t193 int = combine__119(accumulator__120, x29)
            accumulator__120 = t193
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__120
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t215 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__220,
        end_1: end__219,
    }
    var t216 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t215)
    })
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var t221 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    return t221
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env162 closure_env_main_0, sum__11 int, item__12 int) int {
    var t236 int = sum__11 + item__12
    return t236
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env163 closure_env_goml_builtin_range_1) Option__int {
    var current__220 *ref_int_x = env163.current_0
    var end__219 int = env163.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t241 bool = value__221 < end__219
    if t241 {
        var t242 int = value__221 + 1
        ref_set__Ref_3int(current__220, t242)
        var t243 Option__int = Some{
            _0: value__221,
        }
        return t243
    } else {
        return None{}
    }
}

func main() {
    main0()
}
