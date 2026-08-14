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

type Ordering int32

func _goml_m_trait__impl_i_Ready_i_Worker_i_state(self__0 Worker) string {
    var t411 string = self__0.name
    var t412 string = t411 + ":ready"
    return t412
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t415 Worker = self__1.worker
    return t415
}

func main0() struct{} {
    var t417 Worker = Worker{
        name: "build",
    }
    var t418 Station = Station{
        worker: t417,
    }
    var t419 string
    var inline433 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t418)
    var inline434 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline433)
    t419 = inline434
    var inline430 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline430)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
