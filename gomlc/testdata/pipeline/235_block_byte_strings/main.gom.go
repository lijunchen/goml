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
    var t195 int = base__0 + 2
    return t195
}

func loop_answer() int {
    var jp199 int
    var base__1 int = 6
    var t201 int = base__1 * 7
    jp199 = t201
    return jp199
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
    var t203 int = loop_answer()
    var t204 int = value__7 + t203
    var t205 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t206 string = _goml_m_inherent_i_int_i_int_i_to__string(t205)
    println__T_string(t206)
    var t207 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t208 string = _goml_m_inherent_i_int_i_int_i_to__string(t207)
    println__T_string(t208)
    var t209 uint8
    var inline292 int = 0
    var inline293 uint8 = vec_get__Vec_5uint8(plain__2, inline292)
    t209 = inline293
    var t210 int = int(uint8(t209))
    var t211 string
    var inline290 string = _goml_runtime_core_int_to_string(t210)
    t211 = inline290
    var inline287 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline287)
    var t212 uint8
    var inline284 int = 1
    var inline285 uint8 = vec_get__Vec_5uint8(plain__2, inline284)
    t212 = inline285
    var t213 int = int(uint8(t212))
    var t214 string
    var inline282 string = _goml_runtime_core_int_to_string(t213)
    t214 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline279)
    var t215 uint8
    var inline276 int = 2
    var inline277 uint8 = vec_get__Vec_5uint8(plain__2, inline276)
    t215 = inline277
    var t216 int = int(uint8(t215))
    var t217 string
    var inline274 string = _goml_runtime_core_int_to_string(t216)
    t217 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline271)
    var t218 int
    var inline269 int = vec_len__Vec_5uint8(raw__4)
    t218 = inline269
    var t219 string
    var inline267 string = _goml_runtime_core_int_to_string(t218)
    t219 = inline267
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline264)
    var t220 int
    var inline262 int = vec_len__Vec_5uint8(quoted__5)
    t220 = inline262
    var t221 string
    var inline260 string = _goml_runtime_core_int_to_string(t220)
    t221 = inline260
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline257)
    var t222 uint8 = array_get__Array_3_5uint8(fixed__6, 2)
    var t223 int = int(uint8(t222))
    var t224 string
    var inline255 string = _goml_runtime_core_int_to_string(t223)
    t224 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline252)
    var t225 string
    var inline250 string = _goml_runtime_core_int_to_string(t204)
    t225 = inline250
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline247)
    var inline243 string = "block condition"
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline243)
    _goml_runtime_core_string_println(inline244)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t229 string
    t229 = value__1
    _goml_runtime_core_string_println(t229)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t233 int = vec_len__Vec_5uint8(self__189)
    return t233
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t236 string = _goml_runtime_core_int_to_string(self__32)
    return t236
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
