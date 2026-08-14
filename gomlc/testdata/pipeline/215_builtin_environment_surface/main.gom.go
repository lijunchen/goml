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
    var t197 string = _goml_runtime_core_int_to_string(native__0)
    var t198 string = _goml_runtime_core_int8_to_string(small__1)
    var t199 string = t197 + t198
    var t200 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t201 string = t199 + t200
    var t202 string
    var inline327 string = "abcd"
    var inline328 int = 1
    var inline329 int = 3
    var inline330 bool = string_is_char_boundary(inline327, inline328)
    var inline332 bool
    if inline330 {
        var inline335 bool = string_is_char_boundary(inline327, inline329)
        inline332 = inline335
    } else {
        inline332 = false
    }
    if inline332 {
        var inline333 string = _goml_runtime_core_string_byte_slice(inline327, inline328, inline329)
        t202 = inline333
        var text__3 string = t201 + t202
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t203 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t203)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t204 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t204]
        var t205 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t205]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t206 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t206)
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
        var t208 FnIterator__int
        var inline323 int = 0
        var inline324 int = 3
        var inline325 FnIterator__int = __goml_builtin_range(inline323, inline324)
        t208 = inline325
        var t209 closure_env_main_0 = closure_env_main_0{}
        var t210 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t209, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t208, 0, t210)
        var t211 string
        var inline321 string = _goml_runtime_core_bool_to_string(same__5)
        t211 = inline321
        var t212 string = text__3 + t211
        var t213 int32 = received__10._0
        var t214 string
        var inline319 string = _goml_runtime_core_int32_to_string(t213)
        t214 = inline319
        var t215 string = t212 + t214
        var t216 bool = received__10._1
        var t217 string
        var inline317 string = _goml_runtime_core_bool_to_string(t216)
        t217 = inline317
        var t218 string = t215 + t217
        var t219 string
        var inline315 string = _goml_runtime_core_int_to_string(total__13)
        t219 = inline315
        var t220 string = t218 + t219
        var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
        _goml_runtime_core_string_println(inline312)
        return struct{}{}
    } else {
        var inline334 string = _goml_runtime_core_string_byte_slice(inline327, -1, -1)
        t202 = inline334
        var text__3 string = t201 + t202
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t203 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t203)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t204 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t204]
        var t205 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t205]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t206 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t206)
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
        var t208 FnIterator__int
        var inline323 int = 0
        var inline324 int = 3
        var inline325 FnIterator__int = __goml_builtin_range(inline323, inline324)
        t208 = inline325
        var t209 closure_env_main_0 = closure_env_main_0{}
        var t210 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t209, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t208, 0, t210)
        var t211 string
        var inline321 string = _goml_runtime_core_bool_to_string(same__5)
        t211 = inline321
        var t212 string = text__3 + t211
        var t213 int32 = received__10._0
        var t214 string
        var inline319 string = _goml_runtime_core_int32_to_string(t213)
        t214 = inline319
        var t215 string = t212 + t214
        var t216 bool = received__10._1
        var t217 string
        var inline317 string = _goml_runtime_core_bool_to_string(t216)
        t217 = inline317
        var t218 string = t215 + t217
        var t219 string
        var inline315 string = _goml_runtime_core_int_to_string(total__13)
        t219 = inline315
        var t220 string = t218 + t219
        var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
        _goml_runtime_core_string_println(inline312)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr235:
    for {
        var mtmp43 Option__int
        var inline337 func() Option__int = iterator__48.next_fn
        var inline338 Option__int = inline337()
        mtmp43 = inline338
        switch mtmp43.(type) {
        case None:
            break Loop_loop_expr235
        case Some:
            var x44 int = mtmp43.(Some)._0
            var t237 int = combine__50(accumulator__51, x44)
            accumulator__51 = t237
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t266 bool = index__16 < 0
    var jp258 bool
    if t266 {
        jp258 = true
    } else {
        var t267 int
        var inline346 int = _goml_runtime_core_string_len(value__15)
        t267 = inline346
        var t268 bool = index__16 > t267
        jp258 = t268
    }
    if jp258 {
        return false
    } else {
        var t261 int
        var inline350 int = _goml_runtime_core_string_len(value__15)
        t261 = inline350
        var t262 bool = index__16 == t261
        if t262 {
            return true
        } else {
            var t263 uint8
            var inline348 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t263 = inline348
            var t264_rhs uint8 = 192
            var t264 uint8 = t263 & t264_rhs
            var t265 bool = t264 != 128
            return t265
        }
    }
}

func __goml_builtin_range(start__336 int, end__337 int) FnIterator__int {
    var current__338 *ref_int_x = ref__Ref_3int(start__336)
    var t275 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__338,
        end_1: end__337,
    }
    var t276 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t275)
    }
    var inline352 FnIterator__int = FnIterator__int{
        next_fn: t276,
    }
    return inline352
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env194 closure_env_main_0, sum__11 int, item__12 int) int {
    var t303 int = sum__11 + item__12
    return t303
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env195 closure_env_goml_builtin_range_1) Option__int {
    var current__338 *ref_int_x = env195.current_0
    var end__337 int = env195.end_1
    var value__339 int = ref_get__Ref_3int(current__338)
    var t308 bool = value__339 < end__337
    if t308 {
        var t309 int = value__339 + 1
        ref_set__Ref_3int(current__338, t309)
        var t310 Option__int = Some{
            _0: value__339,
        }
        return t310
    } else {
        return None{}
    }
}

func main() {
    main0()
}
