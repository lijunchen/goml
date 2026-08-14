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
        var t199 Result__int32__string = Ok{
            _0: 5,
        }
        return t199
    } else {
        var t200 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t200
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp204 int32
    if flag__1 {
        var commute_field277 int32
        var commute_field279 string
        if fallback__2 {
            commute_field277 = 5
            jp204 = commute_field277
            var t205 int32 = jp204 + 1
            var t206 Result__int32__string = Ok{
                _0: t205,
            }
            return t206
        } else {
            commute_field279 = "bad-branch"
            var t209 Result__int32__string = Err{
                _0: commute_field279,
            }
            return t209
        }
    } else {
        jp204 = 10
        var t205 int32 = jp204 + 1
        var t206 Result__int32__string = Ok{
            _0: t205,
        }
        return t206
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x190 int32 = res__4.(Ok)._0
        var t214 string
        var inline236 string = _goml_runtime_core_int32_to_string(x190)
        t214 = inline236
        var t215 string = "ok=" + t214
        return t215
    case Err:
        var x191 string = res__4.(Err)._0
        var t216 string = "err=" + x191
        return t216
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t218 Result__int32__string = bump(true, true)
    var t219 string = show(t218)
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline274)
    var t220 Result__int32__string = bump(true, false)
    var t221 string
    switch t220.(type) {
    case Ok:
        var inline266 int32 = t220.(Ok)._0
        var inline268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline266)
        var inline269 string = "ok=" + inline268
        t221 = inline269
    case Err:
        var inline270 string = t220.(Err)._0
        var inline272 string = "err=" + inline270
        t221 = inline272
    default:
        panic("non-exhaustive match")
    }
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline263)
    var t222 Result__int32__string
    var inline249 bool = false
    var inline250 bool = false
    var inline252 int32
    if inline249 {
        var inline256 Result__int32__string = parse(inline250)
        switch inline256.(type) {
        case Ok:
            var inline257 int32 = inline256.(Ok)._0
            inline252 = inline257
            var inline254 int32 = inline252 + 1
            var inline255 Result__int32__string = Ok{
                _0: inline254,
            }
            t222 = inline255
            var t223 string
            switch t222.(type) {
            case Ok:
                var inline241 int32 = t222.(Ok)._0
                var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
                var inline244 string = "ok=" + inline243
                t223 = inline244
            case Err:
                var inline245 string = t222.(Err)._0
                var inline247 string = "err=" + inline245
                t223 = inline247
            default:
                panic("non-exhaustive match")
            }
            var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
            _goml_runtime_core_string_println(inline238)
            return struct{}{}
        case Err:
            var inline259 string = inline256.(Err)._0
            var inline261 Result__int32__string = Err{
                _0: inline259,
            }
            t222 = inline261
            var t223 string
            switch t222.(type) {
            case Ok:
                var inline241 int32 = t222.(Ok)._0
                var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
                var inline244 string = "ok=" + inline243
                t223 = inline244
            case Err:
                var inline245 string = t222.(Err)._0
                var inline247 string = "err=" + inline245
                t223 = inline247
            default:
                panic("non-exhaustive match")
            }
            var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
            _goml_runtime_core_string_println(inline238)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline252 = 10
        var inline254 int32 = inline252 + 1
        var inline255 Result__int32__string = Ok{
            _0: inline254,
        }
        t222 = inline255
        var t223 string
        switch t222.(type) {
        case Ok:
            var inline241 int32 = t222.(Ok)._0
            var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
            var inline244 string = "ok=" + inline243
            t223 = inline244
        case Err:
            var inline245 string = t222.(Err)._0
            var inline247 string = "err=" + inline245
            t223 = inline247
        default:
            panic("non-exhaustive match")
        }
        var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
        _goml_runtime_core_string_println(inline238)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__33)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
