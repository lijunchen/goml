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
    var t146 string = _goml_runtime_core_int_to_string(native__0)
    var t147 string = _goml_runtime_core_int8_to_string(small__1)
    var t148 string = t146 + t147
    var t149 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t150 string = t148 + t149
    var t151 string
    var inline277 string = "abcd"
    var inline278 int = 1
    var inline279 int = 3
    var inline280 bool = string_is_char_boundary(inline277, inline278)
    var inline282 bool
    if inline280 {
        var inline285 bool = string_is_char_boundary(inline277, inline279)
        inline282 = inline285
    } else {
        inline282 = false
    }
    if inline282 {
        var inline283 string = _goml_runtime_core_string_byte_slice(inline277, inline278, inline279)
        t151 = inline283
        var text__3 string = t150 + t151
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t152 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t152)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t153 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t153]
        var t154 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t154]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t155 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t155)
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
        var t156 FnIterator__int
        var inline273 int = 0
        var inline274 int = 3
        var inline275 FnIterator__int = __goml_builtin_range(inline273, inline274)
        t156 = inline275
        var t157 closure_env_main_0 = closure_env_main_0{}
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t156, 0, func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t157, p0, p1)
        })
        var t158 string
        var inline271 string = _goml_runtime_core_bool_to_string(same__5)
        t158 = inline271
        var t159 string = text__3 + t158
        var t160 int32 = received__10._0
        var t161 string
        var inline269 string = _goml_runtime_core_int32_to_string(t160)
        t161 = inline269
        var t162 string = t159 + t161
        var t163 bool = received__10._1
        var t164 string
        var inline267 string = _goml_runtime_core_bool_to_string(t163)
        t164 = inline267
        var t165 string = t162 + t164
        var t166 string
        var inline265 string = _goml_runtime_core_int_to_string(total__13)
        t166 = inline265
        var t167 string = t165 + t166
        var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
        _goml_runtime_core_string_println(inline262)
        return struct{}{}
    } else {
        var inline284 string = _goml_runtime_core_string_byte_slice(inline277, -1, -1)
        t151 = inline284
        var text__3 string = t150 + t151
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t152 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t152)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t153 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t153]
        var t154 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t154]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t155 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t155)
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
        var t156 FnIterator__int
        var inline273 int = 0
        var inline274 int = 3
        var inline275 FnIterator__int = __goml_builtin_range(inline273, inline274)
        t156 = inline275
        var t157 closure_env_main_0 = closure_env_main_0{}
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t156, 0, func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t157, p0, p1)
        })
        var t158 string
        var inline271 string = _goml_runtime_core_bool_to_string(same__5)
        t158 = inline271
        var t159 string = text__3 + t158
        var t160 int32 = received__10._0
        var t161 string
        var inline269 string = _goml_runtime_core_int32_to_string(t160)
        t161 = inline269
        var t162 string = t159 + t161
        var t163 bool = received__10._1
        var t164 string
        var inline267 string = _goml_runtime_core_bool_to_string(t163)
        t164 = inline267
        var t165 string = t162 + t164
        var t166 string
        var inline265 string = _goml_runtime_core_int_to_string(total__13)
        t166 = inline265
        var t167 string = t165 + t166
        var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
        _goml_runtime_core_string_println(inline262)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr182:
    for {
        var mtmp43 Option__int
        var inline287 func() Option__int = iterator__48.next_fn
        var inline288 Option__int = inline287()
        mtmp43 = inline288
        switch mtmp43.(type) {
        case None:
            break Loop_loop_expr182
        case Some:
            var x44 int = mtmp43.(Some)._0
            var t184 int = combine__50(accumulator__51, x44)
            accumulator__51 = t184
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t214 bool = index__16 < 0
    var jp205 bool
    if t214 {
        jp205 = true
    } else {
        var t215 int
        var inline295 int = _goml_runtime_core_string_len(value__15)
        t215 = inline295
        var t216 bool = index__16 > t215
        jp205 = t216
    }
    if jp205 {
        return false
    } else {
        var t208 int
        var inline304 int = _goml_runtime_core_string_len(value__15)
        t208 = inline304
        var t209 bool
        var inline302 bool = index__16 == t208
        t209 = inline302
        if t209 {
            return true
        } else {
            var t210 uint8
            var inline300 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t210 = inline300
            var t211_rhs uint8 = 192
            var t211 uint8 = t210 & t211_rhs
            var t212 bool
            var inline297 uint8 = 128
            var inline298 bool = t211 == inline297
            t212 = inline298
            var t213 bool = !t212
            return t213
        }
    }
}

func __goml_builtin_range(start__226 int, end__227 int) FnIterator__int {
    var current__228 *ref_int_x = ref__Ref_3int(start__226)
    var t223 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__228,
        end_1: end__227,
    }
    var t224 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t223)
    })
    return t224
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t241 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t241
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env143 closure_env_main_0, sum__11 int, item__12 int) int {
    var t253 int = sum__11 + item__12
    return t253
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env144 closure_env_goml_builtin_range_1) Option__int {
    var current__228 *ref_int_x = env144.current_0
    var end__227 int = env144.end_1
    var value__229 int = ref_get__Ref_3int(current__228)
    var t258 bool = value__229 < end__227
    if t258 {
        var t259 int = value__229 + 1
        ref_set__Ref_3int(current__228, t259)
        var t260 Option__int = Some{
            _0: value__229,
        }
        return t260
    } else {
        return None{}
    }
}

func main() {
    main0()
}
