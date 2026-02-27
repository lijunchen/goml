package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type closure_env_closure_apply_0 struct {}

type closure_env_global_invoker_1 struct {}

type closure_env_composer_closure_2 struct {}

func double(x__0 int32) int32 {
    var ret12 int32
    ret12 = x__0 * 2
    return ret12
}

func increment(x__1 int32) int32 {
    var ret13 int32
    ret13 = x__1 + 1
    return ret13
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var ret14 int32
    ret14 = f__2(value__3)
    return ret14
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var ret15 int32
    var t7 int32 = g__5(value__6)
    ret15 = f__4(t7)
    return ret15
}

func main0() struct{} {
    var ret16 struct{}
    var local__7 func(int32) int32 = double
    var first__8 int32 = apply_once(local__7, 4)
    var composed__9 int32 = compose(double, increment, first__8)
    var closure_apply__11 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_result__12 int32 = _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(closure_apply__11, composed__9)
    var global_invoker__15 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var invoked_with_global__16 int32 = _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(global_invoker__15, double, 3)
    var composer_closure__18 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composed_by_closure__19 int32 = _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(composer_closure__18, 5)
    var printer__20 func(string) struct{} = string_println
    var t8 string = int32_to_string(composed__9)
    printer__20(t8)
    var t9 string = int32_to_string(closure_result__12)
    printer__20(t9)
    var t10 string = int32_to_string(invoked_with_global__16)
    printer__20(t10)
    var t11 string = int32_to_string(composed_by_closure__19)
    printer__20(t11)
    ret16 = struct{}{}
    return ret16
}

func _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(env4 closure_env_closure_apply_0, value__10 int32) int32 {
    var ret17 int32
    ret17 = apply_once(increment, value__10)
    return ret17
}

func _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(env5 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var ret18 int32
    ret18 = apply_once(func_to_call__13, value__14)
    return ret18
}

func _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(env6 closure_env_composer_closure_2, value__17 int32) int32 {
    var ret19 int32
    ret19 = compose(double, increment, value__17)
    return ret19
}

func main() {
    main0()
}
