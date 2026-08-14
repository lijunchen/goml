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

type Maybe__int interface {
    isMaybe__int()
}

type Maybe__int_None struct {}

func (_ Maybe__int_None) isMaybe__int() {}

type Maybe__int_Some struct {
    _0 int
}

func (_ Maybe__int_Some) isMaybe__int() {}

type Maybe__string interface {
    isMaybe__string()
}

type Maybe__string_None struct {}

func (_ Maybe__string_None) isMaybe__string() {}

type Maybe__string_Some struct {
    _0 string
}

func (_ Maybe__string_Some) isMaybe__string() {}

func main0() struct{} {
    var t192 closure_env_main_0 = closure_env_main_0{}
    var t193 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t192, p0)
    }
    var commute_field230 string
    var inline222 int = 3
    var inline224 string = t193(inline222)
    commute_field230 = inline224
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field230)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env190 closure_env_main_0, item__4 int) string {
    var inline228 string = _goml_runtime_core_int_to_string(item__4)
    return inline228
}

func main() {
    main0()
}
