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
        var t194 Result__int32__string = Ok{
            _0: 5,
        }
        return t194
    } else {
        var t195 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t195
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp199 int32
    if flag__1 {
        var commute_field272 int32
        var commute_field274 string
        if fallback__2 {
            commute_field272 = 5
            jp199 = commute_field272
            var t200 int32 = jp199 + 1
            var t201 Result__int32__string = Ok{
                _0: t200,
            }
            return t201
        } else {
            commute_field274 = "bad-branch"
            var t204 Result__int32__string = Err{
                _0: commute_field274,
            }
            return t204
        }
    } else {
        jp199 = 10
        var t200 int32 = jp199 + 1
        var t201 Result__int32__string = Ok{
            _0: t200,
        }
        return t201
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x185 int32 = res__4.(Ok)._0
        var t209 string
        var inline231 string = _goml_runtime_core_int32_to_string(x185)
        t209 = inline231
        var t210 string = "ok=" + t209
        return t210
    case Err:
        var x186 string = res__4.(Err)._0
        var t211 string = "err=" + x186
        return t211
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t213 Result__int32__string = bump(true, true)
    var t214 string = show(t213)
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline269)
    var t215 Result__int32__string = bump(true, false)
    var t216 string
    switch t215.(type) {
    case Ok:
        var inline261 int32 = t215.(Ok)._0
        var inline263 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline261)
        var inline264 string = "ok=" + inline263
        t216 = inline264
    case Err:
        var inline265 string = t215.(Err)._0
        var inline267 string = "err=" + inline265
        t216 = inline267
    default:
        panic("non-exhaustive match")
    }
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline258)
    var t217 Result__int32__string
    var inline244 bool = false
    var inline245 bool = false
    var inline247 int32
    if inline244 {
        var inline251 Result__int32__string = parse(inline245)
        switch inline251.(type) {
        case Ok:
            var inline252 int32 = inline251.(Ok)._0
            inline247 = inline252
            var inline249 int32 = inline247 + 1
            var inline250 Result__int32__string = Ok{
                _0: inline249,
            }
            t217 = inline250
            var t218 string
            switch t217.(type) {
            case Ok:
                var inline236 int32 = t217.(Ok)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "ok=" + inline238
                t218 = inline239
            case Err:
                var inline240 string = t217.(Err)._0
                var inline242 string = "err=" + inline240
                t218 = inline242
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        case Err:
            var inline254 string = inline251.(Err)._0
            var inline256 Result__int32__string = Err{
                _0: inline254,
            }
            t217 = inline256
            var t218 string
            switch t217.(type) {
            case Ok:
                var inline236 int32 = t217.(Ok)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "ok=" + inline238
                t218 = inline239
            case Err:
                var inline240 string = t217.(Err)._0
                var inline242 string = "err=" + inline240
                t218 = inline242
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline247 = 10
        var inline249 int32 = inline247 + 1
        var inline250 Result__int32__string = Ok{
            _0: inline249,
        }
        t217 = inline250
        var t218 string
        switch t217.(type) {
        case Ok:
            var inline236 int32 = t217.(Ok)._0
            var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
            var inline239 string = "ok=" + inline238
            t218 = inline239
        case Err:
            var inline240 string = t217.(Err)._0
            var inline242 string = "err=" + inline240
            t218 = inline242
        default:
            panic("non-exhaustive match")
        }
        var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
        _goml_runtime_core_string_println(inline233)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__33)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
