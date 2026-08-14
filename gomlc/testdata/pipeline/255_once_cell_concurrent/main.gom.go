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

type Ordering int32

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
    var inline491 int = 2
    var inline492 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline491)
    results__0 = inline492
    var t415 closure_env_main_1 = closure_env_main_1{
        results_0: results__0,
    }
    var t416 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t415)
    }
    go t416()
    var t417 closure_env_main_3 = closure_env_main_3{
        results_0: results__0,
    }
    var t418 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t417)
    }
    go t418()
    var t419 Option__int
    var inline484 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline485 int = inline484._0
    var inline486 bool = inline484._1
    if inline486 {
        var inline489 Option__int = Some{
            _0: inline485,
        }
        t419 = inline489
    } else {
        t419 = None{}
    }
    var first__1 int
    var inline480 int = 0
    switch t419.(type) {
    case None:
        first__1 = inline480
    case Some:
        var inline481 int = t419.(Some)._0
        first__1 = inline481
    default:
        panic("non-exhaustive match")
    }
    var t420 Option__int
    var inline473 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline474 int = inline473._0
    var inline475 bool = inline473._1
    if inline475 {
        var inline478 Option__int = Some{
            _0: inline474,
        }
        t420 = inline478
    } else {
        t420 = None{}
    }
    var second__2 int
    var inline469 int = 0
    switch t420.(type) {
    case None:
        second__2 = inline469
    case Some:
        var inline470 int = t420.(Some)._0
        second__2 = inline470
    default:
        panic("non-exhaustive match")
    }
    var t421 bool = first__1 == second__2
    var t422 string
    var inline467 string = _goml_runtime_core_bool_to_string(t421)
    t422 = inline467
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline464)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env410 closure_env_main_0) int {
    return 41
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env411 closure_env_main_1) struct{} {
    var results__0 chan int = env411.results_0
    var t452 closure_env_main_0 = closure_env_main_0{}
    var t453 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t452)
    }
    var t454 int
    var inline497 int = once_cell_get_or_init__OnceCell__int(VALUE, t453)
    t454 = inline497
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t454)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env412 closure_env_main_2) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env413 closure_env_main_3) struct{} {
    var results__0 chan int = env413.results_0
    var t459 closure_env_main_2 = closure_env_main_2{}
    var t460 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t459)
    }
    var t461 int
    var inline501 int = once_cell_get_or_init__OnceCell__int(VALUE, t460)
    t461 = inline501
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t461)
    return struct{}{}
}

func main() {
    main0()
}
