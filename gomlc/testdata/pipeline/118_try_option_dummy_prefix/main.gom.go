package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func cut_prefix(case_id__0 int32) Option__string {
    var t164 bool
    var inline191 int32 = 0
    var inline192 bool = case_id__0 == inline191
    t164 = inline192
    if t164 {
        var t165 Option__string = Some{
            _0: "ml",
        }
        return t165
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t178 Option__string
    var inline221 int32 = 0
    var inline222 Option__string = cut_prefix(inline221)
    var inline224 string
    switch inline222.(type) {
    case None:
        t178 = None{}
        var t179 string
        switch t178.(type) {
        case None:
            t179 = "none"
        case Some:
            var inline217 string = t178.(Some)._0
            var inline219 string = "some " + inline217
            t179 = inline219
        default:
            panic("non-exhaustive match")
        }
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
        _goml_runtime_core_string_println(inline214)
        var t180 Option__string
        var inline204 int32 = 1
        var inline205 Option__string = cut_prefix(inline204)
        var inline207 string
        switch inline205.(type) {
        case None:
            t180 = None{}
            var t181 string
            switch t180.(type) {
            case None:
                t181 = "none"
            case Some:
                var inline200 string = t180.(Some)._0
                var inline202 string = "some " + inline200
                t181 = inline202
            default:
                panic("non-exhaustive match")
            }
            var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline197)
            return struct{}{}
        case Some:
            var inline211 string = inline205.(Some)._0
            inline207 = inline211
            var inline209 string = inline207 + "!"
            var inline210 Option__string = Some{
                _0: inline209,
            }
            t180 = inline210
            var t181 string
            switch t180.(type) {
            case None:
                t181 = "none"
            case Some:
                var inline200 string = t180.(Some)._0
                var inline202 string = "some " + inline200
                t181 = inline202
            default:
                panic("non-exhaustive match")
            }
            var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline197)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline228 string = inline222.(Some)._0
        inline224 = inline228
        var inline226 string = inline224 + "!"
        var inline227 Option__string = Some{
            _0: inline226,
        }
        t178 = inline227
        var t179 string
        switch t178.(type) {
        case None:
            t179 = "none"
        case Some:
            var inline217 string = t178.(Some)._0
            var inline219 string = "some " + inline217
            t179 = inline219
        default:
            panic("non-exhaustive match")
        }
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
        _goml_runtime_core_string_println(inline214)
        var t180 Option__string
        var inline204 int32 = 1
        var inline205 Option__string = cut_prefix(inline204)
        var inline207 string
        switch inline205.(type) {
        case None:
            t180 = None{}
            var t181 string
            switch t180.(type) {
            case None:
                t181 = "none"
            case Some:
                var inline200 string = t180.(Some)._0
                var inline202 string = "some " + inline200
                t181 = inline202
            default:
                panic("non-exhaustive match")
            }
            var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline197)
            return struct{}{}
        case Some:
            var inline211 string = inline205.(Some)._0
            inline207 = inline211
            var inline209 string = inline207 + "!"
            var inline210 Option__string = Some{
                _0: inline209,
            }
            t180 = inline210
            var t181 string
            switch t180.(type) {
            case None:
                t181 = "none"
            case Some:
                var inline200 string = t180.(Some)._0
                var inline202 string = "some " + inline200
                t181 = inline202
            default:
                panic("non-exhaustive match")
            }
            var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline197)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
