package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_5uint8(arr [3]uint8, index int) uint8 {
    return arr[index]
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

func answer() int {
    var base__0 int = 40
    var t200 int = base__0 + 2
    return t200
}

func loop_answer() int {
    var jp204 int
    var base__1 int = 6
    var t206 int = base__1 * 7
    jp204 = t206
    return jp204
}

func main0() struct{} {
    var plain__2 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{65, 10, 66},
    }
    var empty__3 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{},
    }
    var raw__4 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{114, 97, 119, 32, 92, 110, 32, 98, 121, 116, 101, 115},
    }
    var quoted__5 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{113, 117, 111, 116, 101, 100, 32, 34, 116, 101, 120, 116, 34, 32, 97, 110, 100, 32, 35},
    }
    var fixed__6 [3]uint8 = [3]uint8{65, 66, 67}
    var value__7 int = answer()
    var t208 int = loop_answer()
    var t209 int = value__7 + t208
    var t210 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t211 string = _goml_m_inherent_i_int_i_int_i_to__string(t210)
    println__T_string(t211)
    var t212 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t213 string = _goml_m_inherent_i_int_i_int_i_to__string(t212)
    println__T_string(t213)
    var t214 uint8
    var inline297 int = 0
    var inline298 uint8 = vec_get__Vec_5uint8(plain__2, inline297)
    t214 = inline298
    var t215 int = int(uint8(t214))
    var t216 string
    var inline295 string = _goml_runtime_core_int_to_string(t215)
    t216 = inline295
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline292)
    var t217 uint8
    var inline289 int = 1
    var inline290 uint8 = vec_get__Vec_5uint8(plain__2, inline289)
    t217 = inline290
    var t218 int = int(uint8(t217))
    var t219 string
    var inline287 string = _goml_runtime_core_int_to_string(t218)
    t219 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline284)
    var t220 uint8
    var inline281 int = 2
    var inline282 uint8 = vec_get__Vec_5uint8(plain__2, inline281)
    t220 = inline282
    var t221 int = int(uint8(t220))
    var t222 string
    var inline279 string = _goml_runtime_core_int_to_string(t221)
    t222 = inline279
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline276)
    var t223 int
    var inline274 int = vec_len__Vec_5uint8(raw__4)
    t223 = inline274
    var t224 string
    var inline272 string = _goml_runtime_core_int_to_string(t223)
    t224 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline269)
    var t225 int
    var inline267 int = vec_len__Vec_5uint8(quoted__5)
    t225 = inline267
    var t226 string
    var inline265 string = _goml_runtime_core_int_to_string(t225)
    t226 = inline265
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline262)
    var t227 uint8 = array_get__Array_3_5uint8(fixed__6, 2)
    var t228 int = int(uint8(t227))
    var t229 string
    var inline260 string = _goml_runtime_core_int_to_string(t228)
    t229 = inline260
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline257)
    var t230 string
    var inline255 string = _goml_runtime_core_int_to_string(t209)
    t230 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline252)
    var inline248 string = "block condition"
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
    _goml_runtime_core_string_println(inline249)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t234 string
    t234 = value__1
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t238 int = vec_len__Vec_5uint8(self__189)
    return t238
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t241 string = _goml_runtime_core_int_to_string(self__32)
    return t241
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
