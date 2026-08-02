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

type closure_env_run_0 struct {
    flag_0 bool
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

func main0() struct{} {
    var t183 Result__int32__string
    var inline244 bool = true
    var inline245 closure_env_run_0 = closure_env_run_0{
        flag_0: inline244,
    }
    var inline246 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline245)
    t183 = inline246
    var t184 string
    switch t183.(type) {
    case Ok:
        var inline236 int32 = t183.(Ok)._0
        var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
        var inline239 string = "ok=" + inline238
        t184 = inline239
    case Err:
        var inline240 string = t183.(Err)._0
        var inline242 string = "err=" + inline240
        t184 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline233)
    var t185 Result__int32__string
    var inline229 bool = false
    var inline230 closure_env_run_0 = closure_env_run_0{
        flag_0: inline229,
    }
    var inline231 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline230)
    t185 = inline231
    var t186 string
    switch t185.(type) {
    case Ok:
        var inline221 int32 = t185.(Ok)._0
        var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
        var inline224 string = "ok=" + inline223
        t186 = inline224
    case Err:
        var inline225 string = t185.(Err)._0
        var inline227 string = "err=" + inline225
        t186 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t189 string = _goml_runtime_core_int32_to_string(self__6)
    return t189
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env162 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env162.flag_0
    var mtmp155 Result__int32__string
    if flag__3 {
        var inline252 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp155 = inline252
    } else {
        var inline253 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp155 = inline253
    }
    var jp198 int32
    switch mtmp155.(type) {
    case Ok:
        var x156 int32 = mtmp155.(Ok)._0
        jp198 = x156
        var t199 int32
        var inline249 int32 = 1
        var inline250 int32 = jp198 + inline249
        t199 = inline250
        var t200 Result__int32__string = Ok{
            _0: t199,
        }
        return t200
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t201 Result__int32__string = Err{
            _0: x157,
        }
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
