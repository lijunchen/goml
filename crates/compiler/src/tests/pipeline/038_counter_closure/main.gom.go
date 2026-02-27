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

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_closure_env_next_0_closure_env_reset_1 struct {
    _0 closure_env_next_0
    _1 closure_env_reset_1
}

type closure_env_next_0 struct {
    cell_0 *ref_int32_x
}

type closure_env_reset_1 struct {
    cell_0 *ref_int32_x
}

func make_counter() Tuple2_closure_env_next_0_closure_env_reset_1 {
    var ret146 Tuple2_closure_env_next_0_closure_env_reset_1
    var cell__0 *ref_int32_x = ref__Ref_int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    ret146 = Tuple2_closure_env_next_0_closure_env_reset_1{
        _0: next__2,
        _1: reset__3,
    }
    return ret146
}

func main0() struct{} {
    var ret147 struct{}
    var counter__4 Tuple2_closure_env_next_0_closure_env_reset_1 = make_counter()
    var mtmp69 Tuple2_closure_env_next_0_closure_env_reset_1 = counter__4
    var x70 closure_env_next_0 = mtmp69._0
    var x71 closure_env_reset_1 = mtmp69._1
    var reset__6 closure_env_reset_1 = x71
    var next__5 closure_env_next_0 = x70
    var first__7 int32 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    var second__8 int32 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    _goml_inherent_closure_env_reset_1_closure_env_reset_1_apply(reset__6)
    var third__9 int32 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    var new_counter__10 Tuple2_closure_env_next_0_closure_env_reset_1 = make_counter()
    var mtmp121 Tuple2_closure_env_next_0_closure_env_reset_1 = new_counter__10
    var x122 closure_env_next_0 = mtmp121._0
    var new_next__11 closure_env_next_0 = x122
    var fourth__12 int32 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(new_next__11)
    var t141 string = int32_to_string(first__7)
    string_println(t141)
    var t142 string = int32_to_string(second__8)
    string_println(t142)
    var t143 string = int32_to_string(third__9)
    string_println(t143)
    var t144 string = int32_to_string(fourth__12)
    string_println(t144)
    ret147 = struct{}{}
    return ret147
}

func _goml_inherent_closure_env_next_0_closure_env_next_0_apply(env139 closure_env_next_0) int32 {
    var ret148 int32
    var cell__0 *ref_int32_x = env139.cell_0
    var t145 int32 = ref_get__Ref_int32(cell__0)
    var next__1 int32 = t145 + 1
    ref_set__Ref_int32(cell__0, next__1)
    ret148 = next__1
    return ret148
}

func _goml_inherent_closure_env_reset_1_closure_env_reset_1_apply(env140 closure_env_reset_1) struct{} {
    var ret149 struct{}
    var cell__0 *ref_int32_x = env140.cell_0
    ref_set__Ref_int32(cell__0, 0)
    ret149 = struct{}{}
    return ret149
}

func main() {
    main0()
}
