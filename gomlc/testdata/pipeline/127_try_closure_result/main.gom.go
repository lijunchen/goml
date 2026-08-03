package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    ok_0 bool
    prefix_1 string
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func main0() struct{} {
    var t160 Result__string__string
    var inline203 string = "outer"
    var inline204 bool = true
    var inline205 closure_env_run_0 = closure_env_run_0{
        ok_0: inline204,
        prefix_1: inline203,
    }
    var inline206 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline205)
    t160 = inline206
    var t161 string
    switch t160.(type) {
    case Ok:
        var inline196 string = t160.(Ok)._0
        var inline198 string = "ok " + inline196
        t161 = inline198
    case Err:
        var inline199 string = t160.(Err)._0
        var inline201 string = "err " + inline199
        t161 = inline201
    default:
        panic("non-exhaustive match")
    }
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline193)
    var t162 Result__string__string
    var inline188 string = "outer"
    var inline189 bool = false
    var inline190 closure_env_run_0 = closure_env_run_0{
        ok_0: inline189,
        prefix_1: inline188,
    }
    var inline191 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline190)
    t162 = inline191
    var t163 string
    switch t162.(type) {
    case Ok:
        var inline181 string = t162.(Ok)._0
        var inline183 string = "ok " + inline181
        t163 = inline183
    case Err:
        var inline184 string = t162.(Err)._0
        var inline186 string = "err " + inline184
        t163 = inline186
    default:
        panic("non-exhaustive match")
    }
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline178)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env143 closure_env_run_0) Result__string__string {
    var ok__2 bool = env143.ok_0
    var prefix__1 string = env143.prefix_1
    var mtmp136 Result__string__string
    if ok__2 {
        var inline209 Result__string__string = Ok{
            _0: "body",
        }
        mtmp136 = inline209
    } else {
        var inline210 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp136 = inline210
    }
    var jp172 string
    switch mtmp136.(type) {
    case Ok:
        var x137 string = mtmp136.(Ok)._0
        jp172 = x137
        var t173 string = prefix__1 + ":"
        var t174 string = t173 + jp172
        var t175 Result__string__string = Ok{
            _0: t174,
        }
        return t175
    case Err:
        var x138 string = mtmp136.(Err)._0
        var t176 Result__string__string = Err{
            _0: x138,
        }
        return t176
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
