package main

import (
    _goml_ffi_import_strings_hff6d73a8adbc12e9f5edf8b318fbd31b "strings"
    _goml_ffi_import_runtime_hbfd85f862b823c92c4bdefd661241cfa "runtime"
)

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_ffi_strings_x00_ToUpper_x00__m__z_string_h76369dffc1fe27f425185fa360e330cd(arg0 string) string {
    return _goml_ffi_import_strings_hff6d73a8adbc12e9f5edf8b318fbd31b.ToUpper(arg0)
}

func _goml_ffi_strings_x00_Cut_x00__o__ng_c_bool_q__hbfb59822899b5650c9939dcbef222236(arg0 string, arg1 string) Tuple3_6string_6string_4bool {
    var result0 string
    var result1 string
    var result2 bool
    result0, result1, result2 = _goml_ffi_import_strings_hff6d73a8adbc12e9f5edf8b318fbd31b.Cut(arg0, arg1)
    return Tuple3_6string_6string_4bool{
        _0: result0,
        _1: result1,
        _2: result2,
    }
}

func _goml_ffi_runtime_x00_Gosched_x00_q__m__z_unit_heb04ed8262b1bc7e893c8de929f316b2() struct{} {
    _goml_ffi_import_runtime_hbfd85f862b823c92c4bdefd661241cfa.Gosched()
    return struct{}{}
}

type Tuple3_6string_6string_4bool struct {
    _0 string
    _1 string
    _2 bool
}

func main0() struct{} {
    var t145 string = _goml_ffi_strings_x00_ToUpper_x00__m__z_string_h76369dffc1fe27f425185fa360e330cd("goml")
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline168)
    var mtmp137 Tuple3_6string_6string_4bool = _goml_ffi_strings_x00_Cut_x00__o__ng_c_bool_q__hbfb59822899b5650c9939dcbef222236("left:right", ":")
    var x138 string = mtmp137._0
    var x139 string = mtmp137._1
    var x140 bool = mtmp137._2
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x138)
    _goml_runtime_core_string_println(inline165)
    var inline162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x139)
    _goml_runtime_core_string_println(inline162)
    var inline159 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x140)
    _goml_runtime_core_string_println(inline159)
    _goml_ffi_runtime_x00_Gosched_x00_q__m__z_unit_heb04ed8262b1bc7e893c8de929f316b2()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t157 string = _goml_runtime_core_bool_to_string(self__66)
    return t157
}

func main() {
    main0()
}
