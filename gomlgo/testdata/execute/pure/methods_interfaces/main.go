package main

type Counter struct {
	value int
}

func (counter Counter) Add(amount int) int {
	return counter.value + amount
}

func (counter *Counter) Bump() {
	counter.value++
}

type Adder interface {
	Add(int) int
}

type Inner struct {
	value int
}

func (inner Inner) Sum(amount int) int {
	return inner.value + amount
}

func (inner *Inner) Grow(amount int) int {
	inner.value += amount
	return inner.value
}

type Outer struct {
	Inner
}

type PointerOuter struct {
	*Inner
}

type Summer interface {
	Sum(int) int
}

type Grower interface {
	Grow(int) int
}

func use(adder Adder) int {
	return adder.Add(4)
}

func main() {
	counter := Counter{value: 3}
	println(counter.Add(2))
	counter.Bump()
	println(counter.value)
	var adder Adder = counter
	println(use(adder))
	value, ok := adder.(Counter)
	println(value.value, ok)
	_, pointerOK := adder.(*Counter)
	println(pointerOK)
	var empty any = counter
	asserted := empty.(Counter)
	println(asserted.value)
	add := Counter.Add
	println(add(counter, 5))
	bump := (*Counter).Bump
	bump(&counter)
	println(counter.value)
	boundAdd := counter.Add
	counter.value = 20
	println(boundAdd(1), counter.Add(1))
	boundBump := counter.Bump
	boundBump()
	println(counter.value)
	outer := Outer{Inner: Inner{value: 10}}
	println(outer.Sum(1))
	outer.Grow(2)
	println(outer.Inner.value)
	boundSum := outer.Sum
	outer.Inner.value = 30
	println(boundSum(1), outer.Sum(1))
	boundGrow := outer.Grow
	println(boundGrow(2))
	outerSum := Outer.Sum
	println(outerSum(outer, 3))
	outerGrow := (*Outer).Grow
	println(outerGrow(&outer, 4))
	pointerOuter := PointerOuter{Inner: &Inner{value: 40}}
	println(pointerOuter.Sum(1))
	pointerGrow := pointerOuter.Grow
	println(pointerGrow(2))
	pointerGrowExpression := PointerOuter.Grow
	println(pointerGrowExpression(pointerOuter, 3))
	var summer Summer = outer
	println(summer.Sum(1))
	boundInterfaceSum := summer.Sum
	println(boundInterfaceSum(2))
	interfaceSum := Summer.Sum
	println(interfaceSum(summer, 3))
	var pointerSummer Summer = pointerOuter
	println(pointerSummer.Sum(4))
	var grower Grower = &outer
	println(grower.Grow(1))
	boundInterfaceGrow := grower.Grow
	println(boundInterfaceGrow(1))
	var embeddedGrower Grower = pointerOuter
	println(embeddedGrower.Grow(1))
}
