package main

import (
    _goml_fmt "fmt"
    _goml_sync "sync"
    _goml_runtime_pkg "runtime"
)

func _goml_once_cell_goroutine_id() uint64 {
    var buffer []uint8 = make([]uint8, 64)
    var length int = _goml_runtime_pkg.Stack(buffer, false)
    var index int = 10
    var result uint64 = 0
    for {
        if index >= length {
            break
        }
        if buffer[index] < 48 || buffer[index] > 57 {
            break
        }
        result = result * 10 + uint64(buffer[index] - 48)
        index = index + 1
    }
    return result
}

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

func once_cell_new__OnceCell__int() *OnceCell__int {
    var cell *OnceCell__int = &OnceCell__int{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__int(cell *OnceCell__int, init func() int) int {
    var goroutine uint64 = _goml_once_cell_goroutine_id()
    cell.mutex.Lock()
    for {
        if cell.state == 2 {
            cell.mutex.Unlock()
            return cell.value
        }
        if cell.state == 1 {
            if cell.owner == goroutine {
                cell.mutex.Unlock()
                panic("recursive OnceCell initialization: " + cell.name)
            }
            cell.cond.Wait()
            continue
        }
        cell.state = 1
        cell.owner = goroutine
        cell.mutex.Unlock()
        var initialized int = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {
    results_0 chan int
}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {
    results_0 chan int
}

type OnceCell__int struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value int
    name string
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

var VALUE *OnceCell__int = func() *OnceCell__int {
    var cell *OnceCell__int = once_cell_new__OnceCell__int()
    cell.name = "VALUE"
    return cell
}()

func main0() struct{} {
    var results__0 chan int
    var inline265 int = 2
    var inline266 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline265)
    results__0 = inline266
    var t189 closure_env_main_1 = closure_env_main_1{
        results_0: results__0,
    }
    var t190 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t189)
    }
    go t190()
    var t191 closure_env_main_3 = closure_env_main_3{
        results_0: results__0,
    }
    var t192 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t191)
    }
    go t192()
    var t193 Option__int
    var inline258 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline259 int = inline258._0
    var inline260 bool = inline258._1
    if inline260 {
        var inline263 Option__int = Some{
            _0: inline259,
        }
        t193 = inline263
    } else {
        t193 = None{}
    }
    var first__1 int
    var inline254 int = 0
    switch t193.(type) {
    case None:
        first__1 = inline254
    case Some:
        var inline255 int = t193.(Some)._0
        first__1 = inline255
    default:
        panic("non-exhaustive match")
    }
    var t194 Option__int
    var inline247 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline248 int = inline247._0
    var inline249 bool = inline247._1
    if inline249 {
        var inline252 Option__int = Some{
            _0: inline248,
        }
        t194 = inline252
    } else {
        t194 = None{}
    }
    var second__2 int
    var inline243 int = 0
    switch t194.(type) {
    case None:
        second__2 = inline243
    case Some:
        var inline244 int = t194.(Some)._0
        second__2 = inline244
    default:
        panic("non-exhaustive match")
    }
    var t195 bool = first__1 == second__2
    var t196 string
    var inline241 string = _goml_runtime_core_bool_to_string(t195)
    t196 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline238)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env184 closure_env_main_0) int {
    return 41
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env185 closure_env_main_1) struct{} {
    var results__0 chan int = env185.results_0
    var t226 closure_env_main_0 = closure_env_main_0{}
    var t227 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t226)
    }
    var t228 int
    var inline271 int = once_cell_get_or_init__OnceCell__int(VALUE, t227)
    t228 = inline271
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t228)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env186 closure_env_main_2) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env187 closure_env_main_3) struct{} {
    var results__0 chan int = env187.results_0
    var t233 closure_env_main_2 = closure_env_main_2{}
    var t234 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t233)
    }
    var t235 int
    var inline275 int = once_cell_get_or_init__OnceCell__int(VALUE, t234)
    t235 = inline275
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t235)
    return struct{}{}
}

func main() {
    main0()
}
