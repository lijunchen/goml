package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Worker struct {
    name string
}

type Station struct {
    worker Worker
}

func _goml_m_trait__impl_i_Ready_i_Worker_i_state(self__0 Worker) string {
    var retv24 string
    var t25 string = self__0.name
    var t26 string = t25 + ":ready"
    retv24 = t26
    return retv24
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv28 Worker
    var t29 Worker = self__1.worker
    retv28 = t29
    return retv28
}

func main0() struct{} {
    var t31 Worker = Worker{
        name: "build",
    }
    var t32 Station = Station{
        worker: t31,
    }
    var t33 string = describe__S_Station__T_Worker(t32)
    println__T_string(t33)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv38 string
    var t39 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t40 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t39)
    retv38 = t40
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
