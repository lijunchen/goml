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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
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

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type NumberSource struct {
    value int
}

type closure_env_increment_0 struct {
    captured_0 *ref_int_x
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

type dyn__Source_vtable struct {
    get func(any) int
}

type dyn__Source struct {
    data any
    vtable *dyn__Source_vtable
}

func dyn__Source__wrap__NumberSource__get(self any) int {
    return _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self.(NumberSource))
}

func dyn__Source__vtable__NumberSource() *dyn__Source_vtable {
    return &dyn__Source_vtable{
        get: dyn__Source__wrap__NumberSource__get,
    }
}

func _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self__0 NumberSource) int {
    var t240 int = self__0.value
    return t240
}

func labeled_cleanup() struct{} {
    var inline349 string = "inner cleanup"
    var inline350 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline349)
    _goml_runtime_core_string_println(inline350)
    var inline345 string = "outer cleanup"
    var inline346 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline345)
    _goml_runtime_core_string_println(inline346)
    return struct{}{}
}

func main0() struct{} {
    var t251 NumberSource = NumberSource{
        value: 11,
    }
    var t252 dyn__Source = dyn__Source{
        data: t251,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t253 int
    var inline394 int = t252.vtable.get(t252.data)
    t253 = inline394
    var inline391 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t253)
    _goml_runtime_core_string_println(inline391)
    var x178 int = 1
    var x179 int = 2
    var index__2 int = x178
    var compound_old180 int = index__2
    var t254 int = compound_old180 + x179
    index__2 = t254
    var inline388 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline388)
    var x185 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x185)
    var t256 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t256)
    }
    increment__5()
    var t257 int = ref_get__Ref_3int(captured__4)
    var inline385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t257)
    _goml_runtime_core_string_println(inline385)
    var x193 int = 4
    var count__6 int = x193
    var compound_old194 int = count__6
    var compound_value195 int = 1
    var t308 int = compound_old194 + compound_value195
    count__6 = t308
    var inline353 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline353)
    var values__7 *_goml_vec_int
    var inline383 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline383
    var inline380 int = 6
    vec_push__Vec_3int(values__7, inline380)
    var for_limit200 int = vec_len__Vec_3int(values__7)
    var for_index201 int = 0
    Loop_loop302:
    for {
        var t303 bool = for_index201 < for_limit200
        if t303 {
            var for_item202 int = vec_get__Vec_3int(values__7, for_index201)
            var t304 int = for_index201 + 1
            for_index201 = t304
            var item__8 int = for_item202
            var compound_old204 int = item__8
            var compound_value205 int = 1
            var t305 int = compound_old204 + compound_value205
            item__8 = t305
            var inline356 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline356)
            continue
        } else {
            break Loop_loop302
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root208 Tuple2_3int_3int = legacy__9
    var place209 int = place_root208._0
    var value210 int = 1
    var t260 int = place209 + value210
    var t261 int = place_root208._1
    var t262 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t260,
        _1: t261,
    }
    legacy__9 = t262
    var place_root212 Tuple2_3int_3int = legacy__9
    var place213 int = place_root212._1
    var value214 int = 1
    var t264 int = place_root212._0
    var t265 int = place213 + value214
    var t266 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t264,
        _1: t265,
    }
    legacy__9 = t266
    var t268 int = legacy__9._0
    var t269 int = legacy__9._1
    var t270 int = t268 + t269
    var inline377 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t270)
    _goml_runtime_core_string_println(inline377)
    var steps__10 int = 0
    Loop_loop295:
    for {
        var t296 bool = steps__10 < 3
        if t296 {
            var compound_old217 int = steps__10
            var compound_value218 int = 1
            var t297 int = compound_old217 + compound_value218
            steps__10 = t297
            continue
        } else {
            break Loop_loop295
        }
    }
    var inline374 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline374)
    var seen__11 *ref_int_x
    var inline371 int = 0
    var inline372 *ref_int_x = ref__Ref_3int(inline371)
    seen__11 = inline372
    var for_index223 int = 0
    var for_limit224 int = 3
    Loop_loop281:
    for {
        var t282 bool = for_index223 < for_limit224
        if t282 {
            var for_item225 int = for_index223
            var t283 int = for_index223 + 1
            for_index223 = t283
            var for_index227 int = 0
            var for_limit228 int = 3
            var t293 bool = for_item225 == 1
            Loop_loop285:
            for {
                var t286 bool = for_index227 < for_limit228
                if t286 {
                    var for_item229 int = for_index227
                    var t287 int = for_index227 + 1
                    for_index227 = t287
                    var t288 int
                    var inline361 int = ref_get__Ref_3int(seen__11)
                    t288 = inline361
                    var t289 int = t288 + 1
                    ref_set__Ref_3int(seen__11, t289)
                    var jp292 bool
                    if t293 {
                        var t294 bool = for_item229 == 1
                        jp292 = t294
                    } else {
                        jp292 = false
                    }
                    if jp292 {
                        var t273 int
                        var inline369 int = ref_get__Ref_3int(seen__11)
                        t273 = inline369
                        var inline366 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t273)
                        _goml_runtime_core_string_println(inline366)
                        var jp275 int
                        jp275 = 42
                        var inline363 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp275)
                        _goml_runtime_core_string_println(inline363)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop285
                }
            }
            continue
        } else {
            break Loop_loop281
        }
    }
    var t273 int
    var inline369 int = ref_get__Ref_3int(seen__11)
    t273 = inline369
    var inline366 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t273)
    _goml_runtime_core_string_println(inline366)
    var jp275 int
    jp275 = 42
    var inline363 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp275)
    _goml_runtime_core_string_println(inline363)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t334 string = _goml_runtime_core_int_to_string(self__67)
    return t334
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env237 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env237.captured_0
    var compound_old187 int = ref_get__Ref_3int(captured__4)
    var compound_value188 int = 1
    var t342 int = compound_old187 + compound_value188
    ref_set__Ref_3int(captured__4, t342)
    return struct{}{}
}

func main() {
    main0()
}
