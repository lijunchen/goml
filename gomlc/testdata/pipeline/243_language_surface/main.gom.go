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
    var inline348 string = "inner cleanup"
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline348)
    _goml_runtime_core_string_println(inline349)
    var inline344 string = "outer cleanup"
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline344)
    _goml_runtime_core_string_println(inline345)
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
    var inline400 int = t252.vtable.get(t252.data)
    t253 = inline400
    var inline397 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t253)
    _goml_runtime_core_string_println(inline397)
    var x178 int = 1
    var x179 int = 2
    var index__2 int = x178
    var compound_old180 int = index__2
    var t254 int = compound_old180 + x179
    index__2 = t254
    var inline394 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline394)
    var x185 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x185)
    var inline388 int = ref_get__Ref_3int(captured__4)
    var inline389 int = 1
    var inline390 int = inline388 + inline389
    ref_set__Ref_3int(captured__4, inline390)
    var t256 int = ref_get__Ref_3int(captured__4)
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t256)
    _goml_runtime_core_string_println(inline384)
    var x193 int = 4
    var count__6 int = x193
    var compound_old194 int = count__6
    var compound_value195 int = 1
    var t307 int = compound_old194 + compound_value195
    count__6 = t307
    var inline352 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline352)
    var values__7 *_goml_vec_int
    var inline382 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline382
    var inline379 int = 6
    vec_push__Vec_3int(values__7, inline379)
    var for_limit200 int = vec_len__Vec_3int(values__7)
    var for_index201 int = 0
    Loop_loop301:
    for {
        var t302 bool = for_index201 < for_limit200
        if t302 {
            var for_item202 int = vec_get__Vec_3int(values__7, for_index201)
            var t303 int = for_index201 + 1
            for_index201 = t303
            var item__8 int = for_item202
            var compound_old204 int = item__8
            var compound_value205 int = 1
            var t304 int = compound_old204 + compound_value205
            item__8 = t304
            var inline355 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline355)
            continue
        } else {
            break Loop_loop301
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root208 Tuple2_3int_3int = legacy__9
    var place209 int = place_root208._0
    var value210 int = 1
    var t259 int = place209 + value210
    var t260 int = place_root208._1
    var t261 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t259,
        _1: t260,
    }
    legacy__9 = t261
    var place_root212 Tuple2_3int_3int = legacy__9
    var place213 int = place_root212._1
    var value214 int = 1
    var t263 int = place_root212._0
    var t264 int = place213 + value214
    var t265 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t263,
        _1: t264,
    }
    legacy__9 = t265
    var t267 int = legacy__9._0
    var t268 int = legacy__9._1
    var t269 int = t267 + t268
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t269)
    _goml_runtime_core_string_println(inline376)
    var steps__10 int = 0
    Loop_loop294:
    for {
        var t295 bool = steps__10 < 3
        if t295 {
            var compound_old217 int = steps__10
            var compound_value218 int = 1
            var t296 int = compound_old217 + compound_value218
            steps__10 = t296
            continue
        } else {
            break Loop_loop294
        }
    }
    var inline373 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline373)
    var seen__11 *ref_int_x
    var inline370 int = 0
    var inline371 *ref_int_x = ref__Ref_3int(inline370)
    seen__11 = inline371
    var for_index223 int = 0
    var for_limit224 int = 3
    Loop_loop280:
    for {
        var t281 bool = for_index223 < for_limit224
        if t281 {
            var for_item225 int = for_index223
            var t282 int = for_index223 + 1
            for_index223 = t282
            var for_index227 int = 0
            var for_limit228 int = 3
            var t292 bool = for_item225 == 1
            Loop_loop284:
            for {
                var t285 bool = for_index227 < for_limit228
                if t285 {
                    var for_item229 int = for_index227
                    var t286 int = for_index227 + 1
                    for_index227 = t286
                    var t287 int
                    var inline360 int = ref_get__Ref_3int(seen__11)
                    t287 = inline360
                    var t288 int = t287 + 1
                    ref_set__Ref_3int(seen__11, t288)
                    var jp291 bool
                    if t292 {
                        var t293 bool = for_item229 == 1
                        jp291 = t293
                    } else {
                        jp291 = false
                    }
                    if jp291 {
                        var t272 int
                        var inline368 int = ref_get__Ref_3int(seen__11)
                        t272 = inline368
                        var inline365 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t272)
                        _goml_runtime_core_string_println(inline365)
                        var jp274 int
                        jp274 = 42
                        var inline362 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp274)
                        _goml_runtime_core_string_println(inline362)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop284
                }
            }
            continue
        } else {
            break Loop_loop280
        }
    }
    var t272 int
    var inline368 int = ref_get__Ref_3int(seen__11)
    t272 = inline368
    var inline365 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t272)
    _goml_runtime_core_string_println(inline365)
    var jp274 int
    jp274 = 42
    var inline362 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp274)
    _goml_runtime_core_string_println(inline362)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t333 string = _goml_runtime_core_int_to_string(self__69)
    return t333
}

func main() {
    main0()
}
