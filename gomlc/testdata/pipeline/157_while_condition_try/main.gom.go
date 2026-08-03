package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func run_some() Option__int32 {
    var i__2 *ref_int32_x
    var inline245 int32 = 0
    var inline246 *ref_int32_x = ref__Ref_5int32(inline245)
    i__2 = inline246
    var total__3 *ref_int32_x
    var inline242 int32 = 0
    var inline243 *ref_int32_x = ref__Ref_5int32(inline242)
    total__3 = inline243
    Loop_loop167:
    for {
        var t168 int32
        var inline238 int32 = ref_get__Ref_5int32(i__2)
        t168 = inline238
        var mtmp136 Option__bool
        var inline234 bool = t168 < 3
        if inline234 {
            var inline235 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp136 = inline235
        } else {
            var inline236 Option__bool = Option__bool_Some{
                _0: false,
            }
            mtmp136 = inline236
        }
        var jp170 bool
        switch mtmp136.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x137 bool = mtmp136.(Option__bool_Some)._0
            jp170 = x137
            if jp170 {
                var t171 int32
                var inline232 int32 = ref_get__Ref_5int32(total__3)
                t171 = inline232
                var t172 int32
                var inline230 int32 = ref_get__Ref_5int32(i__2)
                t172 = inline230
                var t173 int32 = t171 + t172
                ref_set__Ref_5int32(total__3, t173)
                var t174 int32
                var inline226 int32 = ref_get__Ref_5int32(i__2)
                t174 = inline226
                var t175 int32 = t174 + 1
                ref_set__Ref_5int32(i__2, t175)
                continue
            } else {
                break Loop_loop167
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t165 int32
    var inline240 int32 = ref_get__Ref_5int32(total__3)
    t165 = inline240
    var t166 Option__int32 = Option__int32_Some{
        _0: t165,
    }
    return t166
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline268 int32 = 0
    var inline269 *ref_int32_x = ref__Ref_5int32(inline268)
    i__4 = inline269
    var total__5 *ref_int32_x
    var inline265 int32 = 0
    var inline266 *ref_int32_x = ref__Ref_5int32(inline265)
    total__5 = inline266
    Loop_loop181:
    for {
        var t182 int32
        var inline261 int32 = ref_get__Ref_5int32(i__4)
        t182 = inline261
        var mtmp141 Option__bool
        var inline258 bool = t182 < 2
        if inline258 {
            var inline259 Option__bool = Option__bool_Some{
                _0: true,
            }
            mtmp141 = inline259
        } else {
            mtmp141 = Option__bool_None{}
        }
        var jp184 bool
        switch mtmp141.(type) {
        case Option__bool_None:
            return Option__int32_None{}
        case Option__bool_Some:
            var x142 bool = mtmp141.(Option__bool_Some)._0
            jp184 = x142
            if jp184 {
                var t185 int32
                var inline256 int32 = ref_get__Ref_5int32(total__5)
                t185 = inline256
                var t186 int32
                var inline254 int32 = ref_get__Ref_5int32(i__4)
                t186 = inline254
                var t187 int32 = t185 + t186
                ref_set__Ref_5int32(total__5, t187)
                var t188 int32
                var inline250 int32 = ref_get__Ref_5int32(i__4)
                t188 = inline250
                var t189 int32 = t188 + 1
                ref_set__Ref_5int32(i__4, t189)
                continue
            } else {
                break Loop_loop181
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t179 int32
    var inline263 int32 = ref_get__Ref_5int32(total__5)
    t179 = inline263
    var t180 Option__int32 = Option__int32_Some{
        _0: t179,
    }
    return t180
}

func main0() struct{} {
    var t197 Option__int32 = run_some()
    var t198 string
    switch t197.(type) {
    case Option__int32_None:
        t198 = "none"
    case Option__int32_Some:
        var inline284 int32 = t197.(Option__int32_Some)._0
        var inline286 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline284)
        var inline287 string = "some=" + inline286
        t198 = inline287
    default:
        panic("non-exhaustive match")
    }
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline281)
    var t199 Option__int32 = run_none()
    var t200 string
    switch t199.(type) {
    case Option__int32_None:
        t200 = "none"
    case Option__int32_Some:
        var inline276 int32 = t199.(Option__int32_Some)._0
        var inline278 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline276)
        var inline279 string = "some=" + inline278
        t200 = inline279
    default:
        panic("non-exhaustive match")
    }
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__35)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
