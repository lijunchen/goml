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
    var t145 bool
    var inline172 int32 = 0
    var inline173 bool = case_id__0 == inline172
    t145 = inline173
    if t145 {
        var t146 Option__string = Some{
            _0: "ml",
        }
        return t146
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t159 Option__string
    var inline202 int32 = 0
    var inline203 Option__string = cut_prefix(inline202)
    var inline205 string
    switch inline203.(type) {
    case None:
        t159 = None{}
        var t160 string
        switch t159.(type) {
        case None:
            t160 = "none"
        case Some:
            var inline198 string = t159.(Some)._0
            var inline200 string = "some " + inline198
            t160 = inline200
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
        _goml_runtime_core_string_println(inline195)
        var t161 Option__string
        var inline185 int32 = 1
        var inline186 Option__string = cut_prefix(inline185)
        var inline188 string
        switch inline186.(type) {
        case None:
            t161 = None{}
            var t162 string
            switch t161.(type) {
            case None:
                t162 = "none"
            case Some:
                var inline181 string = t161.(Some)._0
                var inline183 string = "some " + inline181
                t162 = inline183
            default:
                panic("non-exhaustive match")
            }
            var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline178)
            return struct{}{}
        case Some:
            var inline192 string = inline186.(Some)._0
            inline188 = inline192
            var inline190 string = inline188 + "!"
            var inline191 Option__string = Some{
                _0: inline190,
            }
            t161 = inline191
            var t162 string
            switch t161.(type) {
            case None:
                t162 = "none"
            case Some:
                var inline181 string = t161.(Some)._0
                var inline183 string = "some " + inline181
                t162 = inline183
            default:
                panic("non-exhaustive match")
            }
            var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline178)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline209 string = inline203.(Some)._0
        inline205 = inline209
        var inline207 string = inline205 + "!"
        var inline208 Option__string = Some{
            _0: inline207,
        }
        t159 = inline208
        var t160 string
        switch t159.(type) {
        case None:
            t160 = "none"
        case Some:
            var inline198 string = t159.(Some)._0
            var inline200 string = "some " + inline198
            t160 = inline200
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
        _goml_runtime_core_string_println(inline195)
        var t161 Option__string
        var inline185 int32 = 1
        var inline186 Option__string = cut_prefix(inline185)
        var inline188 string
        switch inline186.(type) {
        case None:
            t161 = None{}
            var t162 string
            switch t161.(type) {
            case None:
                t162 = "none"
            case Some:
                var inline181 string = t161.(Some)._0
                var inline183 string = "some " + inline181
                t162 = inline183
            default:
                panic("non-exhaustive match")
            }
            var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline178)
            return struct{}{}
        case Some:
            var inline192 string = inline186.(Some)._0
            inline188 = inline192
            var inline190 string = inline188 + "!"
            var inline191 Option__string = Some{
                _0: inline190,
            }
            t161 = inline191
            var t162 string
            switch t161.(type) {
            case None:
                t162 = "none"
            case Some:
                var inline181 string = t161.(Some)._0
                var inline183 string = "some " + inline181
                t162 = inline183
            default:
                panic("non-exhaustive match")
            }
            var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline178)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
