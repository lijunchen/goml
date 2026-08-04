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
    var t204 int = self__0.value
    return t204
}

func read(source__1 dyn__Source) int {
    var t207 int = source__1.vtable.get(source__1.data)
    return t207
}

func labeled_cleanup() struct{} {
    var inline312 string = "inner cleanup"
    var inline313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline312)
    _goml_runtime_core_string_println(inline313)
    var inline308 string = "outer cleanup"
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline308)
    _goml_runtime_core_string_println(inline309)
    return struct{}{}
}

func main0() struct{} {
    var t215 NumberSource = NumberSource{
        value: 11,
    }
    var t216 dyn__Source = dyn__Source{
        data: t215,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t217 int = read(t216)
    println__T_int(t217)
    var x142 int = 1
    var x143 int = 2
    var index__2 int = x142
    var compound_old144 int = index__2
    var t218 int = compound_old144 + x143
    index__2 = t218
    var inline364 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline364)
    var x149 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x149)
    var inline358 int = ref_get__Ref_3int(captured__4)
    var inline359 int = 1
    var inline360 int = inline358 + inline359
    ref_set__Ref_3int(captured__4, inline360)
    var t220 int = ref_get__Ref_3int(captured__4)
    var inline354 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t220)
    _goml_runtime_core_string_println(inline354)
    var x157 int = 4
    var count__6 int = x157
    var compound_old158 int = count__6
    var compound_value159 int = 1
    var t271 int = compound_old158 + compound_value159
    count__6 = t271
    var inline316 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline316)
    var values__7 *_goml_vec_int
    var inline352 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline352
    var inline349 int = 6
    vec_push__Vec_3int(values__7, inline349)
    var for_limit164 int = vec_len__Vec_3int(values__7)
    var for_index165 int = 0
    Loop_loop265:
    for {
        var t266 bool = for_index165 < for_limit164
        if t266 {
            var for_item166 int = vec_get__Vec_3int(values__7, for_index165)
            var t267 int = for_index165 + 1
            for_index165 = t267
            var item__8 int = for_item166
            var compound_old168 int = item__8
            var compound_value169 int = 1
            var t268 int = compound_old168 + compound_value169
            item__8 = t268
            var inline319 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline319)
            continue
        } else {
            break Loop_loop265
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root172 Tuple2_3int_3int = legacy__9
    var place173 int = place_root172._0
    var value174 int = 1
    var t223 int = place173 + value174
    var t224 int = place_root172._1
    var t225 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t223,
        _1: t224,
    }
    legacy__9 = t225
    var place_root176 Tuple2_3int_3int = legacy__9
    var place177 int = place_root176._1
    var value178 int = 1
    var t227 int = place_root176._0
    var t228 int = place177 + value178
    var t229 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t227,
        _1: t228,
    }
    legacy__9 = t229
    var t231 int = legacy__9._0
    var t232 int = legacy__9._1
    var t233 int = t231 + t232
    var inline346 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t233)
    _goml_runtime_core_string_println(inline346)
    var steps__10 int = 0
    Loop_loop258:
    for {
        var t259 bool = steps__10 < 3
        if t259 {
            var compound_old181 int = steps__10
            var compound_value182 int = 1
            var t260 int = compound_old181 + compound_value182
            steps__10 = t260
            continue
        } else {
            break Loop_loop258
        }
    }
    var inline343 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline343)
    var seen__11 *ref_int_x
    var inline340 int = 0
    var inline341 *ref_int_x = ref__Ref_3int(inline340)
    seen__11 = inline341
    var for_index187 int = 0
    var for_limit188 int = 3
    Loop_loop244:
    for {
        var t245 bool = for_index187 < for_limit188
        if t245 {
            var for_item189 int = for_index187
            var t246 int = for_index187 + 1
            for_index187 = t246
            var for_index191 int = 0
            var for_limit192 int = 3
            Loop_loop248:
            for {
                var t249 bool = for_index191 < for_limit192
                if t249 {
                    var for_item193 int = for_index191
                    var t250 int = for_index191 + 1
                    for_index191 = t250
                    var t251 int
                    var inline330 int = ref_get__Ref_3int(seen__11)
                    t251 = inline330
                    var t252 int = t251 + 1
                    ref_set__Ref_3int(seen__11, t252)
                    var t256 bool
                    var inline325 int = 1
                    var inline326 bool = for_item189 == inline325
                    t256 = inline326
                    var jp255 bool
                    if t256 {
                        var inline322 int = 1
                        var inline323 bool = for_item193 == inline322
                        jp255 = inline323
                    } else {
                        jp255 = false
                    }
                    if jp255 {
                        var t236 int
                        var inline338 int = ref_get__Ref_3int(seen__11)
                        t236 = inline338
                        var inline335 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t236)
                        _goml_runtime_core_string_println(inline335)
                        var jp238 int
                        jp238 = 42
                        var inline332 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp238)
                        _goml_runtime_core_string_println(inline332)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop248
                }
            }
            continue
        } else {
            break Loop_loop244
        }
    }
    var t236 int
    var inline338 int = ref_get__Ref_3int(seen__11)
    t236 = inline338
    var inline335 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t236)
    _goml_runtime_core_string_println(inline335)
    var jp238 int
    jp238 = 42
    var inline332 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp238)
    _goml_runtime_core_string_println(inline332)
    labeled_cleanup()
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t278 string
    var inline368 string = _goml_runtime_core_int_to_string(value__31)
    t278 = inline368
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t300 string = _goml_runtime_core_int_to_string(self__69)
    return t300
}

func main() {
    main0()
}
