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

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t146 Result__int32__string = Ok{
            _0: 41,
        }
        return t146
    } else {
        var t147 Result__int32__string = Err{
            _0: "bad",
        }
        return t147
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp136 Result__int32__string
    if flag__1 {
        var inline175 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp136 = inline175
    } else {
        var inline176 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp136 = inline176
    }
    var jp151 int32
    switch mtmp136.(type) {
    case Err:
        var x137 string = mtmp136.(Err)._0
        var t154 Result__int32__string = Err{
            _0: x137,
        }
        return t154
    case Ok:
        var x138 int32 = mtmp136.(Ok)._0
        jp151 = x138
        var t152 int32 = jp151 + 1
        var t153 Result__int32__string = Ok{
            _0: t152,
        }
        return t153
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t161 Result__int32__string = compute(true)
    var t162 string
    switch t161.(type) {
    case Err:
        var inline205 string = t161.(Err)._0
        t162 = inline205
    case Ok:
        var inline207 int32 = t161.(Ok)._0
        var inline209 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline207)
        t162 = inline209
    default:
        panic("non-exhaustive match")
    }
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
    _goml_runtime_core_string_println(inline202)
    var t163 Result__int32__string
    var inline189 bool = false
    var inline190 Result__int32__string = parse(inline189)
    var inline192 int32
    switch inline190.(type) {
    case Err:
        var inline196 string = inline190.(Err)._0
        var inline198 Result__int32__string = Err{
            _0: inline196,
        }
        t163 = inline198
        var t164 string
        switch t163.(type) {
        case Err:
            var inline183 string = t163.(Err)._0
            t164 = inline183
        case Ok:
            var inline185 int32 = t163.(Ok)._0
            var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
            t164 = inline187
        default:
            panic("non-exhaustive match")
        }
        var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
        _goml_runtime_core_string_println(inline180)
        return struct{}{}
    case Ok:
        var inline199 int32 = inline190.(Ok)._0
        inline192 = inline199
        var inline194 int32 = inline192 + 1
        var inline195 Result__int32__string = Ok{
            _0: inline194,
        }
        t163 = inline195
        var t164 string
        switch t163.(type) {
        case Err:
            var inline183 string = t163.(Err)._0
            t164 = inline183
        case Ok:
            var inline185 int32 = t163.(Ok)._0
            var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
            t164 = inline187
        default:
            panic("non-exhaustive match")
        }
        var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
        _goml_runtime_core_string_println(inline180)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t168 string = _goml_runtime_core_int32_to_string(self__35)
    return t168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
