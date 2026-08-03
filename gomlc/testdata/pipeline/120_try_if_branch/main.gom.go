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
        var t148 Result__int32__string = Ok{
            _0: 5,
        }
        return t148
    } else {
        var t149 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t149
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp153 int32
    if flag__1 {
        var commute_field226 int32
        var commute_field228 string
        if fallback__2 {
            commute_field226 = 5
            jp153 = commute_field226
            var t154 int32 = jp153 + 1
            var t155 Result__int32__string = Ok{
                _0: t154,
            }
            return t155
        } else {
            commute_field228 = "bad-branch"
            var t158 Result__int32__string = Err{
                _0: commute_field228,
            }
            return t158
        }
    } else {
        jp153 = 10
        var t154 int32 = jp153 + 1
        var t155 Result__int32__string = Ok{
            _0: t154,
        }
        return t155
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x139 int32 = res__4.(Ok)._0
        var t163 string
        var inline185 string = _goml_runtime_core_int32_to_string(x139)
        t163 = inline185
        var t164 string = "ok=" + t163
        return t164
    case Err:
        var x140 string = res__4.(Err)._0
        var t165 string = "err=" + x140
        return t165
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t167 Result__int32__string = bump(true, true)
    var t168 string = show(t167)
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t168)
    _goml_runtime_core_string_println(inline223)
    var t169 Result__int32__string = bump(true, false)
    var t170 string
    switch t169.(type) {
    case Ok:
        var inline215 int32 = t169.(Ok)._0
        var inline217 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline215)
        var inline218 string = "ok=" + inline217
        t170 = inline218
    case Err:
        var inline219 string = t169.(Err)._0
        var inline221 string = "err=" + inline219
        t170 = inline221
    default:
        panic("non-exhaustive match")
    }
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline212)
    var t171 Result__int32__string
    var inline198 bool = false
    var inline199 bool = false
    var inline201 int32
    if inline198 {
        var inline205 Result__int32__string = parse(inline199)
        switch inline205.(type) {
        case Ok:
            var inline206 int32 = inline205.(Ok)._0
            inline201 = inline206
            var inline203 int32 = inline201 + 1
            var inline204 Result__int32__string = Ok{
                _0: inline203,
            }
            t171 = inline204
            var t172 string
            switch t171.(type) {
            case Ok:
                var inline190 int32 = t171.(Ok)._0
                var inline192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline190)
                var inline193 string = "ok=" + inline192
                t172 = inline193
            case Err:
                var inline194 string = t171.(Err)._0
                var inline196 string = "err=" + inline194
                t172 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
            _goml_runtime_core_string_println(inline187)
            return struct{}{}
        case Err:
            var inline208 string = inline205.(Err)._0
            var inline210 Result__int32__string = Err{
                _0: inline208,
            }
            t171 = inline210
            var t172 string
            switch t171.(type) {
            case Ok:
                var inline190 int32 = t171.(Ok)._0
                var inline192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline190)
                var inline193 string = "ok=" + inline192
                t172 = inline193
            case Err:
                var inline194 string = t171.(Err)._0
                var inline196 string = "err=" + inline194
                t172 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
            _goml_runtime_core_string_println(inline187)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline201 = 10
        var inline203 int32 = inline201 + 1
        var inline204 Result__int32__string = Ok{
            _0: inline203,
        }
        t171 = inline204
        var t172 string
        switch t171.(type) {
        case Ok:
            var inline190 int32 = t171.(Ok)._0
            var inline192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline190)
            var inline193 string = "ok=" + inline192
            t172 = inline193
        case Err:
            var inline194 string = t171.(Err)._0
            var inline196 string = "err=" + inline194
            t172 = inline196
        default:
            panic("non-exhaustive match")
        }
        var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
        _goml_runtime_core_string_println(inline187)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t175 string = _goml_runtime_core_int32_to_string(self__35)
    return t175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
