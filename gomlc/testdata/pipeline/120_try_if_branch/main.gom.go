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

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t189 Result__int32__string = Ok{
            _0: 5,
        }
        return t189
    } else {
        var t190 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t190
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp194 int32
    if flag__1 {
        var commute_field267 int32
        var commute_field269 string
        if fallback__2 {
            commute_field267 = 5
            jp194 = commute_field267
            var t195 int32 = jp194 + 1
            var t196 Result__int32__string = Ok{
                _0: t195,
            }
            return t196
        } else {
            commute_field269 = "bad-branch"
            var t199 Result__int32__string = Err{
                _0: commute_field269,
            }
            return t199
        }
    } else {
        jp194 = 10
        var t195 int32 = jp194 + 1
        var t196 Result__int32__string = Ok{
            _0: t195,
        }
        return t196
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x180 int32 = res__4.(Ok)._0
        var t204 string
        var inline226 string = _goml_runtime_core_int32_to_string(x180)
        t204 = inline226
        var t205 string = "ok=" + t204
        return t205
    case Err:
        var x181 string = res__4.(Err)._0
        var t206 string = "err=" + x181
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t208 Result__int32__string = bump(true, true)
    var t209 string = show(t208)
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline264)
    var t210 Result__int32__string = bump(true, false)
    var t211 string
    switch t210.(type) {
    case Ok:
        var inline256 int32 = t210.(Ok)._0
        var inline258 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline256)
        var inline259 string = "ok=" + inline258
        t211 = inline259
    case Err:
        var inline260 string = t210.(Err)._0
        var inline262 string = "err=" + inline260
        t211 = inline262
    default:
        panic("non-exhaustive match")
    }
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline253)
    var t212 Result__int32__string
    var inline239 bool = false
    var inline240 bool = false
    var inline242 int32
    if inline239 {
        var inline246 Result__int32__string = parse(inline240)
        switch inline246.(type) {
        case Ok:
            var inline247 int32 = inline246.(Ok)._0
            inline242 = inline247
            var inline244 int32 = inline242 + 1
            var inline245 Result__int32__string = Ok{
                _0: inline244,
            }
            t212 = inline245
            var t213 string
            switch t212.(type) {
            case Ok:
                var inline231 int32 = t212.(Ok)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "ok=" + inline233
                t213 = inline234
            case Err:
                var inline235 string = t212.(Err)._0
                var inline237 string = "err=" + inline235
                t213 = inline237
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        case Err:
            var inline249 string = inline246.(Err)._0
            var inline251 Result__int32__string = Err{
                _0: inline249,
            }
            t212 = inline251
            var t213 string
            switch t212.(type) {
            case Ok:
                var inline231 int32 = t212.(Ok)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "ok=" + inline233
                t213 = inline234
            case Err:
                var inline235 string = t212.(Err)._0
                var inline237 string = "err=" + inline235
                t213 = inline237
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline242 = 10
        var inline244 int32 = inline242 + 1
        var inline245 Result__int32__string = Ok{
            _0: inline244,
        }
        t212 = inline245
        var t213 string
        switch t212.(type) {
        case Ok:
            var inline231 int32 = t212.(Ok)._0
            var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
            var inline234 string = "ok=" + inline233
            t213 = inline234
        case Err:
            var inline235 string = t212.(Err)._0
            var inline237 string = "err=" + inline235
            t213 = inline237
        default:
            panic("non-exhaustive match")
        }
        var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
        _goml_runtime_core_string_println(inline228)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t216 string = _goml_runtime_core_int32_to_string(self__35)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
