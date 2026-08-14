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
    var t255 int = self__0.value
    return t255
}

func labeled_cleanup() struct{} {
    var inline364 string = "inner cleanup"
    var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline364)
    _goml_runtime_core_string_println(inline365)
    var inline360 string = "outer cleanup"
    var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline360)
    _goml_runtime_core_string_println(inline361)
    return struct{}{}
}

func main0() struct{} {
    var t266 NumberSource = NumberSource{
        value: 11,
    }
    var t267 dyn__Source = dyn__Source{
        data: t266,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t268 int
    var inline409 int = t267.vtable.get(t267.data)
    t268 = inline409
    var inline406 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t268)
    _goml_runtime_core_string_println(inline406)
    var x193 int = 1
    var x194 int = 2
    var index__2 int = x193
    var compound_old195 int = index__2
    var t269 int = compound_old195 + x194
    index__2 = t269
    var inline403 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline403)
    var x200 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x200)
    var t271 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t271)
    }
    increment__5()
    var t272 int = ref_get__Ref_3int(captured__4)
    var inline400 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t272)
    _goml_runtime_core_string_println(inline400)
    var x208 int = 4
    var count__6 int = x208
    var compound_old209 int = count__6
    var compound_value210 int = 1
    var t323 int = compound_old209 + compound_value210
    count__6 = t323
    var inline368 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline368)
    var values__7 *_goml_vec_int
    var inline398 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline398
    var inline395 int = 6
    vec_push__Vec_3int(values__7, inline395)
    var for_limit215 int = vec_len__Vec_3int(values__7)
    var for_index216 int = 0
    Loop_loop317:
    for {
        var t318 bool = for_index216 < for_limit215
        if t318 {
            var for_item217 int = vec_get__Vec_3int(values__7, for_index216)
            var t319 int = for_index216 + 1
            for_index216 = t319
            var item__8 int = for_item217
            var compound_old219 int = item__8
            var compound_value220 int = 1
            var t320 int = compound_old219 + compound_value220
            item__8 = t320
            var inline371 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline371)
            continue
        } else {
            break Loop_loop317
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root223 Tuple2_3int_3int = legacy__9
    var place224 int = place_root223._0
    var value225 int = 1
    var t275 int = place224 + value225
    var t276 int = place_root223._1
    var t277 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t275,
        _1: t276,
    }
    legacy__9 = t277
    var place_root227 Tuple2_3int_3int = legacy__9
    var place228 int = place_root227._1
    var value229 int = 1
    var t279 int = place_root227._0
    var t280 int = place228 + value229
    var t281 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t279,
        _1: t280,
    }
    legacy__9 = t281
    var t283 int = legacy__9._0
    var t284 int = legacy__9._1
    var t285 int = t283 + t284
    var inline392 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t285)
    _goml_runtime_core_string_println(inline392)
    var steps__10 int = 0
    Loop_loop310:
    for {
        var t311 bool = steps__10 < 3
        if t311 {
            var compound_old232 int = steps__10
            var compound_value233 int = 1
            var t312 int = compound_old232 + compound_value233
            steps__10 = t312
            continue
        } else {
            break Loop_loop310
        }
    }
    var inline389 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline389)
    var seen__11 *ref_int_x
    var inline386 int = 0
    var inline387 *ref_int_x = ref__Ref_3int(inline386)
    seen__11 = inline387
    var for_index238 int = 0
    var for_limit239 int = 3
    Loop_loop296:
    for {
        var t297 bool = for_index238 < for_limit239
        if t297 {
            var for_item240 int = for_index238
            var t298 int = for_index238 + 1
            for_index238 = t298
            var for_index242 int = 0
            var for_limit243 int = 3
            var t308 bool = for_item240 == 1
            Loop_loop300:
            for {
                var t301 bool = for_index242 < for_limit243
                if t301 {
                    var for_item244 int = for_index242
                    var t302 int = for_index242 + 1
                    for_index242 = t302
                    var t303 int
                    var inline376 int = ref_get__Ref_3int(seen__11)
                    t303 = inline376
                    var t304 int = t303 + 1
                    ref_set__Ref_3int(seen__11, t304)
                    var jp307 bool
                    if t308 {
                        var t309 bool = for_item244 == 1
                        jp307 = t309
                    } else {
                        jp307 = false
                    }
                    if jp307 {
                        var t288 int
                        var inline384 int = ref_get__Ref_3int(seen__11)
                        t288 = inline384
                        var inline381 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t288)
                        _goml_runtime_core_string_println(inline381)
                        var jp290 int
                        jp290 = 42
                        var inline378 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp290)
                        _goml_runtime_core_string_println(inline378)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop300
                }
            }
            continue
        } else {
            break Loop_loop296
        }
    }
    var t288 int
    var inline384 int = ref_get__Ref_3int(seen__11)
    t288 = inline384
    var inline381 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t288)
    _goml_runtime_core_string_println(inline381)
    var jp290 int
    jp290 = 42
    var inline378 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp290)
    _goml_runtime_core_string_println(inline378)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t349 string = _goml_runtime_core_int_to_string(self__67)
    return t349
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env252 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env252.captured_0
    var compound_old202 int = ref_get__Ref_3int(captured__4)
    var compound_value203 int = 1
    var t357 int = compound_old202 + compound_value203
    ref_set__Ref_3int(captured__4, t357)
    return struct{}{}
}

func main() {
    main0()
}
