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
    var t7 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t7 = x__0 * 2
            return t7
        default:
            panic("invalid pc")
        }
    }
}

func increment(x__1 int32) int32 {
    var t8 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = x__1 + 1
            return t8
        default:
            panic("invalid pc")
        }
    }
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var t9 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t9 = f__2(value__3)
            return t9
        default:
            panic("invalid pc")
        }
    }
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var t10 int32
    var t11 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t10 = g__5(value__6)
            t11 = f__4(t10)
            return t11
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var local__7 func(int32) int32
    var first__8 int32
    var composed__9 int32
    var closure_apply__11 closure_env_closure_apply_0
    var closure_result__12 int32
    var global_invoker__15 closure_env_global_invoker_1
    var invoked_with_global__16 int32
    var composer_closure__18 closure_env_composer_closure_2
    var composed_by_closure__19 int32
    var printer__20 func(string) struct{}
    var t12 string
    var mtmp0 struct{}
    var t13 string
    var mtmp1 struct{}
    var t14 string
    var mtmp2 struct{}
    var t15 string
    var mtmp3 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            local__7 = double
            first__8 = apply_once(local__7, 4)
            composed__9 = compose(double, increment, first__8)
            closure_apply__11 = closure_env_closure_apply_0{}
            closure_result__12 = _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(closure_apply__11, composed__9)
            global_invoker__15 = closure_env_global_invoker_1{}
            invoked_with_global__16 = _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(global_invoker__15, double, 3)
            composer_closure__18 = closure_env_composer_closure_2{}
            composed_by_closure__19 = _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(composer_closure__18, 5)
            printer__20 = string_println
            t12 = int32_to_string(composed__9)
            printer__20(t12)
            t13 = int32_to_string(closure_result__12)
            printer__20(t13)
            t14 = int32_to_string(invoked_with_global__16)
            printer__20(t14)
            t15 = int32_to_string(composed_by_closure__19)
            printer__20(t15)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(env4 closure_env_closure_apply_0, value__10 int32) int32 {
    var t16 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t16 = apply_once(increment, value__10)
            return t16
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(env5 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var t17 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t17 = apply_once(func_to_call__13, value__14)
            return t17
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(env6 closure_env_composer_closure_2, value__17 int32) int32 {
    var t18 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t18 = compose(double, increment, value__17)
            return t18
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
