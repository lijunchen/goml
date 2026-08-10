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
    var t182 string = _goml_runtime_core_int_to_string(native__0)
    var t183 string = _goml_runtime_core_int8_to_string(small__1)
    var t184 string = t182 + t183
    var t185 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t186 string = t184 + t185
    var t187 string
    var inline312 string = "abcd"
    var inline313 int = 1
    var inline314 int = 3
    var inline315 bool = string_is_char_boundary(inline312, inline313)
    var inline317 bool
    if inline315 {
        var inline320 bool = string_is_char_boundary(inline312, inline314)
        inline317 = inline320
    } else {
        inline317 = false
    }
    if inline317 {
        var inline318 string = _goml_runtime_core_string_byte_slice(inline312, inline313, inline314)
        t187 = inline318
        var text__3 string = t186 + t187
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t188 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t188)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t189 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t189]
        var t190 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t190]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t191 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t191)
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
        var t193 FnIterator__int
        var inline308 int = 0
        var inline309 int = 3
        var inline310 FnIterator__int = __goml_builtin_range(inline308, inline309)
        t193 = inline310
        var t194 closure_env_main_0 = closure_env_main_0{}
        var t195 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t194, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t193, 0, t195)
        var t196 string
        var inline306 string = _goml_runtime_core_bool_to_string(same__5)
        t196 = inline306
        var t197 string = text__3 + t196
        var t198 int32 = received__10._0
        var t199 string
        var inline304 string = _goml_runtime_core_int32_to_string(t198)
        t199 = inline304
        var t200 string = t197 + t199
        var t201 bool = received__10._1
        var t202 string
        var inline302 string = _goml_runtime_core_bool_to_string(t201)
        t202 = inline302
        var t203 string = t200 + t202
        var t204 string
        var inline300 string = _goml_runtime_core_int_to_string(total__13)
        t204 = inline300
        var t205 string = t203 + t204
        var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline297)
        return struct{}{}
    } else {
        var inline319 string = _goml_runtime_core_string_byte_slice(inline312, -1, -1)
        t187 = inline319
        var text__3 string = t186 + t187
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t188 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t188)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t189 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t189]
        var t190 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t190]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t191 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t191)
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
        var t193 FnIterator__int
        var inline308 int = 0
        var inline309 int = 3
        var inline310 FnIterator__int = __goml_builtin_range(inline308, inline309)
        t193 = inline310
        var t194 closure_env_main_0 = closure_env_main_0{}
        var t195 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t194, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t193, 0, t195)
        var t196 string
        var inline306 string = _goml_runtime_core_bool_to_string(same__5)
        t196 = inline306
        var t197 string = text__3 + t196
        var t198 int32 = received__10._0
        var t199 string
        var inline304 string = _goml_runtime_core_int32_to_string(t198)
        t199 = inline304
        var t200 string = t197 + t199
        var t201 bool = received__10._1
        var t202 string
        var inline302 string = _goml_runtime_core_bool_to_string(t201)
        t202 = inline302
        var t203 string = t200 + t202
        var t204 string
        var inline300 string = _goml_runtime_core_int_to_string(total__13)
        t204 = inline300
        var t205 string = t203 + t204
        var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline297)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr220:
    for {
        var mtmp43 Option__int
        var inline322 func() Option__int = iterator__48.next_fn
        var inline323 Option__int = inline322()
        mtmp43 = inline323
        switch mtmp43.(type) {
        case None:
            break Loop_loop_expr220
        case Some:
            var x44 int = mtmp43.(Some)._0
            var t222 int = combine__50(accumulator__51, x44)
            accumulator__51 = t222
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t251 bool = index__16 < 0
    var jp243 bool
    if t251 {
        jp243 = true
    } else {
        var t252 int
        var inline331 int = _goml_runtime_core_string_len(value__15)
        t252 = inline331
        var t253 bool = index__16 > t252
        jp243 = t253
    }
    if jp243 {
        return false
    } else {
        var t246 int
        var inline335 int = _goml_runtime_core_string_len(value__15)
        t246 = inline335
        var t247 bool = index__16 == t246
        if t247 {
            return true
        } else {
            var t248 uint8
            var inline333 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t248 = inline333
            var t249_rhs uint8 = 192
            var t249 uint8 = t248 & t249_rhs
            var t250 bool = t249 != 128
            return t250
        }
    }
}

func __goml_builtin_range(start__318 int, end__319 int) FnIterator__int {
    var current__320 *ref_int_x = ref__Ref_3int(start__318)
    var t260 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__320,
        end_1: end__319,
    }
    var t261 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t260)
    }
    var inline337 FnIterator__int = FnIterator__int{
        next_fn: t261,
    }
    return inline337
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env179 closure_env_main_0, sum__11 int, item__12 int) int {
    var t288 int = sum__11 + item__12
    return t288
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env180 closure_env_goml_builtin_range_1) Option__int {
    var current__320 *ref_int_x = env180.current_0
    var end__319 int = env180.end_1
    var value__321 int = ref_get__Ref_3int(current__320)
    var t293 bool = value__321 < end__319
    if t293 {
        var t294 int = value__321 + 1
        ref_set__Ref_3int(current__320, t294)
        var t295 Option__int = Some{
            _0: value__321,
        }
        return t295
    } else {
        return None{}
    }
}

func main() {
    main0()
}
