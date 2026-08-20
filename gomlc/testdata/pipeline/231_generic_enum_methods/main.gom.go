package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_main_0 struct {}

type Ordering int32

type Maybe__int struct {
    _tag int32
    _v1_0 int
}

type Maybe__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var t413 closure_env_main_0 = closure_env_main_0{}
    var t414 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t413, p0)
    }
    var commute_field451 string
    var inline443 int = 3
    var inline445 string = t414(inline443)
    commute_field451 = inline445
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field451)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env411 closure_env_main_0, item__4 int) string {
    var inline449 string = _goml_runtime_core_int_to_string(item__4)
    return inline449
}

func main() {
    main0()
}
