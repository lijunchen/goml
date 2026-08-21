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
    var t414 string = self__0.name
    var t415 string = t414 + ":ready"
    return t415
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t418 Worker = self__1.worker
    return t418
}

func main0() struct{} {
    var t420 Worker = Worker{
        name: "build",
    }
    var t421 Station = Station{
        worker: t420,
    }
    var t422 string
    var inline436 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t421)
    var inline437 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline436)
    t422 = inline437
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline433)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
