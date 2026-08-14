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
    var t250 int = self__0.value
    return t250
}

func labeled_cleanup() struct{} {
    var inline359 string = "inner cleanup"
    var inline360 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline359)
    _goml_runtime_core_string_println(inline360)
    var inline355 string = "outer cleanup"
    var inline356 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline355)
    _goml_runtime_core_string_println(inline356)
    return struct{}{}
}

func main0() struct{} {
    var t261 NumberSource = NumberSource{
        value: 11,
    }
    var t262 dyn__Source = dyn__Source{
        data: t261,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t263 int
    var inline404 int = t262.vtable.get(t262.data)
    t263 = inline404
    var inline401 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t263)
    _goml_runtime_core_string_println(inline401)
    var x188 int = 1
    var x189 int = 2
    var index__2 int = x188
    var compound_old190 int = index__2
    var t264 int = compound_old190 + x189
    index__2 = t264
    var inline398 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline398)
    var x195 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x195)
    var t266 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t266)
    }
    increment__5()
    var t267 int = ref_get__Ref_3int(captured__4)
    var inline395 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t267)
    _goml_runtime_core_string_println(inline395)
    var x203 int = 4
    var count__6 int = x203
    var compound_old204 int = count__6
    var compound_value205 int = 1
    var t318 int = compound_old204 + compound_value205
    count__6 = t318
    var inline363 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline363)
    var values__7 *_goml_vec_int
    var inline393 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline393
    var inline390 int = 6
    vec_push__Vec_3int(values__7, inline390)
    var for_limit210 int = vec_len__Vec_3int(values__7)
    var for_index211 int = 0
    Loop_loop312:
    for {
        var t313 bool = for_index211 < for_limit210
        if t313 {
            var for_item212 int = vec_get__Vec_3int(values__7, for_index211)
            var t314 int = for_index211 + 1
            for_index211 = t314
            var item__8 int = for_item212
            var compound_old214 int = item__8
            var compound_value215 int = 1
            var t315 int = compound_old214 + compound_value215
            item__8 = t315
            var inline366 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline366)
            continue
        } else {
            break Loop_loop312
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root218 Tuple2_3int_3int = legacy__9
    var place219 int = place_root218._0
    var value220 int = 1
    var t270 int = place219 + value220
    var t271 int = place_root218._1
    var t272 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t270,
        _1: t271,
    }
    legacy__9 = t272
    var place_root222 Tuple2_3int_3int = legacy__9
    var place223 int = place_root222._1
    var value224 int = 1
    var t274 int = place_root222._0
    var t275 int = place223 + value224
    var t276 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t274,
        _1: t275,
    }
    legacy__9 = t276
    var t278 int = legacy__9._0
    var t279 int = legacy__9._1
    var t280 int = t278 + t279
    var inline387 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t280)
    _goml_runtime_core_string_println(inline387)
    var steps__10 int = 0
    Loop_loop305:
    for {
        var t306 bool = steps__10 < 3
        if t306 {
            var compound_old227 int = steps__10
            var compound_value228 int = 1
            var t307 int = compound_old227 + compound_value228
            steps__10 = t307
            continue
        } else {
            break Loop_loop305
        }
    }
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline384)
    var seen__11 *ref_int_x
    var inline381 int = 0
    var inline382 *ref_int_x = ref__Ref_3int(inline381)
    seen__11 = inline382
    var for_index233 int = 0
    var for_limit234 int = 3
    Loop_loop291:
    for {
        var t292 bool = for_index233 < for_limit234
        if t292 {
            var for_item235 int = for_index233
            var t293 int = for_index233 + 1
            for_index233 = t293
            var for_index237 int = 0
            var for_limit238 int = 3
            var t303 bool = for_item235 == 1
            Loop_loop295:
            for {
                var t296 bool = for_index237 < for_limit238
                if t296 {
                    var for_item239 int = for_index237
                    var t297 int = for_index237 + 1
                    for_index237 = t297
                    var t298 int
                    var inline371 int = ref_get__Ref_3int(seen__11)
                    t298 = inline371
                    var t299 int = t298 + 1
                    ref_set__Ref_3int(seen__11, t299)
                    var jp302 bool
                    if t303 {
                        var t304 bool = for_item239 == 1
                        jp302 = t304
                    } else {
                        jp302 = false
                    }
                    if jp302 {
                        var t283 int
                        var inline379 int = ref_get__Ref_3int(seen__11)
                        t283 = inline379
                        var inline376 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t283)
                        _goml_runtime_core_string_println(inline376)
                        var jp285 int
                        jp285 = 42
                        var inline373 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp285)
                        _goml_runtime_core_string_println(inline373)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop295
                }
            }
            continue
        } else {
            break Loop_loop291
        }
    }
    var t283 int
    var inline379 int = ref_get__Ref_3int(seen__11)
    t283 = inline379
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t283)
    _goml_runtime_core_string_println(inline376)
    var jp285 int
    jp285 = 42
    var inline373 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp285)
    _goml_runtime_core_string_println(inline373)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t344 string = _goml_runtime_core_int_to_string(self__67)
    return t344
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env247 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env247.captured_0
    var compound_old197 int = ref_get__Ref_3int(captured__4)
    var compound_value198 int = 1
    var t352 int = compound_old197 + compound_value198
    ref_set__Ref_3int(captured__4, t352)
    return struct{}{}
}

func main() {
    main0()
}
