package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

func add_after_match(flag__0 bool) int32 {
    var jp146 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp146 = 7
        var t147 int32 = jp146 + 1
        return t147
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t157 int32 = add_after_match(false)
    var inline229 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t157)
    _goml_runtime_core_string_println(inline229)
    var t158 int32
    var inline223 bool = true
    var inline225 int32
    switch inline223 {
    case true:
        t158 = 5
        var inline220 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t158)
        _goml_runtime_core_string_println(inline220)
        var t159 string
        var inline215 bool = false
        var inline217 int
        switch inline215 {
        case true:
            t159 = "early"
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
            _goml_runtime_core_string_println(inline212)
            var t160 string
            var inline207 bool = true
            var inline209 int
            switch inline207 {
            case true:
                t160 = "early"
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            case false:
                inline209 = 7
                var inline210 string = _goml_m_inherent_i_int_i_int_i_to__string(inline209)
                t160 = inline210
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline217 = 7
            var inline218 string = _goml_m_inherent_i_int_i_int_i_to__string(inline217)
            t159 = inline218
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
            _goml_runtime_core_string_println(inline212)
            var t160 string
            var inline207 bool = true
            var inline209 int
            switch inline207 {
            case true:
                t160 = "early"
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            case false:
                inline209 = 7
                var inline210 string = _goml_m_inherent_i_int_i_int_i_to__string(inline209)
                t160 = inline210
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline225 = 7
        var inline227 int32 = inline225 + 1
        t158 = inline227
        var inline220 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t158)
        _goml_runtime_core_string_println(inline220)
        var t159 string
        var inline215 bool = false
        var inline217 int
        switch inline215 {
        case true:
            t159 = "early"
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
            _goml_runtime_core_string_println(inline212)
            var t160 string
            var inline207 bool = true
            var inline209 int
            switch inline207 {
            case true:
                t160 = "early"
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            case false:
                inline209 = 7
                var inline210 string = _goml_m_inherent_i_int_i_int_i_to__string(inline209)
                t160 = inline210
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline217 = 7
            var inline218 string = _goml_m_inherent_i_int_i_int_i_to__string(inline217)
            t159 = inline218
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
            _goml_runtime_core_string_println(inline212)
            var t160 string
            var inline207 bool = true
            var inline209 int
            switch inline207 {
            case true:
                t160 = "early"
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            case false:
                inline209 = 7
                var inline210 string = _goml_m_inherent_i_int_i_int_i_to__string(inline209)
                t160 = inline210
                var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
                _goml_runtime_core_string_println(inline204)
                var t161 int32
                var inline200 bool = false
                var inline201 closure_env_f_0 = closure_env_f_0{}
                var inline202 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline201, inline200)
                t161 = inline202
                var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t161)
                _goml_runtime_core_string_println(inline197)
                var t162 int32
                var inline193 bool = true
                var inline194 closure_env_f_0 = closure_env_f_0{}
                var inline195 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline194, inline193)
                t162 = inline195
                var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
                _goml_runtime_core_string_println(inline190)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t165 string = _goml_runtime_core_int_to_string(self__34)
    return t165
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t174 string = _goml_runtime_core_int32_to_string(self__72)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env142 closure_env_f_0, inner__4 bool) int32 {
    var jp180 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp180 = 4
        var t181 int32 = jp180 + 3
        return t181
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
