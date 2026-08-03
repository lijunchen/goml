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
    var t187 string = _goml_runtime_core_int_to_string(native__0)
    var t188 string = _goml_runtime_core_int8_to_string(small__1)
    var t189 string = t187 + t188
    var t190 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t191 string = t189 + t190
    var t192 string
    var inline318 string = "abcd"
    var inline319 int = 1
    var inline320 int = 3
    var inline321 bool = string_is_char_boundary(inline318, inline319)
    var inline323 bool
    if inline321 {
        var inline326 bool = string_is_char_boundary(inline318, inline320)
        inline323 = inline326
    } else {
        inline323 = false
    }
    if inline323 {
        var inline324 string = _goml_runtime_core_string_byte_slice(inline318, inline319, inline320)
        t192 = inline324
        var text__3 string = t191 + t192
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t193 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t193)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t194 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t194]
        var t195 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t195]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t196 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t196)
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
        var t197 FnIterator__int
        var inline314 int = 0
        var inline315 int = 3
        var inline316 FnIterator__int = __goml_builtin_range(inline314, inline315)
        t197 = inline316
        var t198 closure_env_main_0 = closure_env_main_0{}
        var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t197, 0, func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t198, p0, p1)
        })
        var t199 string
        var inline312 string = _goml_runtime_core_bool_to_string(same__5)
        t199 = inline312
        var t200 string = text__3 + t199
        var t201 int32 = received__10._0
        var t202 string
        var inline310 string = _goml_runtime_core_int32_to_string(t201)
        t202 = inline310
        var t203 string = t200 + t202
        var t204 bool = received__10._1
        var t205 string
        var inline308 string = _goml_runtime_core_bool_to_string(t204)
        t205 = inline308
        var t206 string = t203 + t205
        var t207 string
        var inline306 string = _goml_runtime_core_int_to_string(total__13)
        t207 = inline306
        var t208 string = t206 + t207
        var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
        _goml_runtime_core_string_println(inline303)
        return struct{}{}
    } else {
        var inline325 string = _goml_runtime_core_string_byte_slice(inline318, -1, -1)
        t192 = inline325
        var text__3 string = t191 + t192
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t193 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t193)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t194 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t194]
        var t195 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t195]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t196 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t196)
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
        var t197 FnIterator__int
        var inline314 int = 0
        var inline315 int = 3
        var inline316 FnIterator__int = __goml_builtin_range(inline314, inline315)
        t197 = inline316
        var t198 closure_env_main_0 = closure_env_main_0{}
        var total__13 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t197, 0, func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t198, p0, p1)
        })
        var t199 string
        var inline312 string = _goml_runtime_core_bool_to_string(same__5)
        t199 = inline312
        var t200 string = text__3 + t199
        var t201 int32 = received__10._0
        var t202 string
        var inline310 string = _goml_runtime_core_int32_to_string(t201)
        t202 = inline310
        var t203 string = t200 + t202
        var t204 bool = received__10._1
        var t205 string
        var inline308 string = _goml_runtime_core_bool_to_string(t204)
        t205 = inline308
        var t206 string = t203 + t205
        var t207 string
        var inline306 string = _goml_runtime_core_int_to_string(total__13)
        t207 = inline306
        var t208 string = t206 + t207
        var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
        _goml_runtime_core_string_println(inline303)
        return struct{}{}
    }
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__146 FnIterator__int, initial__147 int, combine__148 func(int, int) int) int {
    var accumulator__149 int = initial__147
    Loop_loop_expr223:
    for {
        var mtmp50 Option__int
        var inline328 func() Option__int = iterator__146.next_fn
        var inline329 Option__int = inline328()
        mtmp50 = inline329
        switch mtmp50.(type) {
        case None:
            break Loop_loop_expr223
        case Some:
            var x51 int = mtmp50.(Some)._0
            var t225 int = combine__148(accumulator__149, x51)
            accumulator__149 = t225
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__149
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t255 bool = index__16 < 0
    var jp246 bool
    if t255 {
        jp246 = true
    } else {
        var t256 int
        var inline336 int = _goml_runtime_core_string_len(value__15)
        t256 = inline336
        var t257 bool = index__16 > t256
        jp246 = t257
    }
    if jp246 {
        return false
    } else {
        var t249 int
        var inline345 int = _goml_runtime_core_string_len(value__15)
        t249 = inline345
        var t250 bool
        var inline343 bool = index__16 == t249
        t250 = inline343
        if t250 {
            return true
        } else {
            var t251 uint8
            var inline341 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t251 = inline341
            var t252_rhs uint8 = 192
            var t252 uint8 = t251 & t252_rhs
            var t253 bool
            var inline338 uint8 = 128
            var inline339 bool = t252 == inline338
            t253 = inline339
            var t254 bool = !t253
            return t254
        }
    }
}

func __goml_builtin_range(start__247 int, end__248 int) FnIterator__int {
    var current__249 *ref_int_x = ref__Ref_3int(start__247)
    var t264 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__249,
        end_1: end__248,
    }
    var t265 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t264)
    })
    return t265
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t282 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t282
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env184 closure_env_main_0, sum__11 int, item__12 int) int {
    var t294 int = sum__11 + item__12
    return t294
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env185 closure_env_goml_builtin_range_1) Option__int {
    var current__249 *ref_int_x = env185.current_0
    var end__248 int = env185.end_1
    var value__250 int = ref_get__Ref_3int(current__249)
    var t299 bool = value__250 < end__248
    if t299 {
        var t300 int = value__250 + 1
        ref_set__Ref_3int(current__249, t300)
        var t301 Option__int = Some{
            _0: value__250,
        }
        return t301
    } else {
        return None{}
    }
}

func main() {
    main0()
}
