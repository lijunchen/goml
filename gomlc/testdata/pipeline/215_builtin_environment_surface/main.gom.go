package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
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
    var t192 string = _goml_runtime_core_int_to_string(native__0)
    var t193 string = _goml_runtime_core_int8_to_string(small__1)
    var t194 string = t192 + t193
    var t195 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t196 string = t194 + t195
    var t197 string
    var inline322 string = "abcd"
    var inline323 int = 1
    var inline324 int = 3
    var inline325 bool = string_is_char_boundary(inline322, inline323)
    var inline327 bool
    if inline325 {
        var inline330 bool = string_is_char_boundary(inline322, inline324)
        inline327 = inline330
    } else {
        inline327 = false
    }
    if inline327 {
        var inline328 string = _goml_runtime_core_string_byte_slice(inline322, inline323, inline324)
        t197 = inline328
        var text__3 string = t196 + t197
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t198 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t198)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t199 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t199]
        var t200 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t200]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t201 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t201)
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
        var t203 FnIterator__int
        var inline318 int = 0
        var inline319 int = 3
        var inline320 FnIterator__int = __goml_builtin_range(inline318, inline319)
        t203 = inline320
        var t204 closure_env_main_0 = closure_env_main_0{}
        var t205 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t204, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t203, 0, t205)
        var t206 string
        var inline316 string = _goml_runtime_core_bool_to_string(same__5)
        t206 = inline316
        var t207 string = text__3 + t206
        var t208 int32 = received__10._0
        var t209 string
        var inline314 string = _goml_runtime_core_int32_to_string(t208)
        t209 = inline314
        var t210 string = t207 + t209
        var t211 bool = received__10._1
        var t212 string
        var inline312 string = _goml_runtime_core_bool_to_string(t211)
        t212 = inline312
        var t213 string = t210 + t212
        var t214 string
        var inline310 string = _goml_runtime_core_int_to_string(total__13)
        t214 = inline310
        var t215 string = t213 + t214
        var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline307)
        return struct{}{}
    } else {
        var inline329 string = _goml_runtime_core_string_byte_slice(inline322, -1, -1)
        t197 = inline329
        var text__3 string = t196 + t197
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t198 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t198)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t199 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t199]
        var t200 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t200]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t201 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t201)
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
        var t203 FnIterator__int
        var inline318 int = 0
        var inline319 int = 3
        var inline320 FnIterator__int = __goml_builtin_range(inline318, inline319)
        t203 = inline320
        var t204 closure_env_main_0 = closure_env_main_0{}
        var t205 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t204, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t203, 0, t205)
        var t206 string
        var inline316 string = _goml_runtime_core_bool_to_string(same__5)
        t206 = inline316
        var t207 string = text__3 + t206
        var t208 int32 = received__10._0
        var t209 string
        var inline314 string = _goml_runtime_core_int32_to_string(t208)
        t209 = inline314
        var t210 string = t207 + t209
        var t211 bool = received__10._1
        var t212 string
        var inline312 string = _goml_runtime_core_bool_to_string(t211)
        t212 = inline312
        var t213 string = t210 + t212
        var t214 string
        var inline310 string = _goml_runtime_core_int_to_string(total__13)
        t214 = inline310
        var t215 string = t213 + t214
        var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline307)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr230:
    for {
        var mtmp43 Option__int
        var inline332 func() Option__int = iterator__48.next_fn
        var inline333 Option__int = inline332()
        mtmp43 = inline333
        switch mtmp43.(type) {
        case None:
            break Loop_loop_expr230
        case Some:
            var x44 int = mtmp43.(Some)._0
            var t232 int = combine__50(accumulator__51, x44)
            accumulator__51 = t232
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t261 bool = index__16 < 0
    var jp253 bool
    if t261 {
        jp253 = true
    } else {
        var t262 int
        var inline341 int = _goml_runtime_core_string_len(value__15)
        t262 = inline341
        var t263 bool = index__16 > t262
        jp253 = t263
    }
    if jp253 {
        return false
    } else {
        var t256 int
        var inline345 int = _goml_runtime_core_string_len(value__15)
        t256 = inline345
        var t257 bool = index__16 == t256
        if t257 {
            return true
        } else {
            var t258 uint8
            var inline343 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t258 = inline343
            var t259_rhs uint8 = 192
            var t259 uint8 = t258 & t259_rhs
            var t260 bool = t259 != 128
            return t260
        }
    }
}

func __goml_builtin_range(start__333 int, end__334 int) FnIterator__int {
    var current__335 *ref_int_x = ref__Ref_3int(start__333)
    var t270 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__335,
        end_1: end__334,
    }
    var t271 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t270)
    }
    var inline347 FnIterator__int = FnIterator__int{
        next_fn: t271,
    }
    return inline347
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env189 closure_env_main_0, sum__11 int, item__12 int) int {
    var t298 int = sum__11 + item__12
    return t298
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env190 closure_env_goml_builtin_range_1) Option__int {
    var current__335 *ref_int_x = env190.current_0
    var end__334 int = env190.end_1
    var value__336 int = ref_get__Ref_3int(current__335)
    var t303 bool = value__336 < end__334
    if t303 {
        var t304 int = value__336 + 1
        ref_set__Ref_3int(current__335, t304)
        var t305 Option__int = Some{
            _0: value__336,
        }
        return t305
    } else {
        return None{}
    }
}

func main() {
    main0()
}
