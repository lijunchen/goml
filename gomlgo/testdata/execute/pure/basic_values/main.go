package main

func pair(value int) (int, int) {
	return value, value + 1
}

func main() {
	first, second := pair(4)
	var unsigned uint8 = 255
	unsigned++
	var signed int8 = 127
	signed++
	converted := int16(signed)
	left := 1.5
	right := left * 2
	zero := 0.0
	negativeZero := -zero
	nan := zero / zero
	complexLeft := complex(1.0, 2.0)
	complexRight := complex(3.0, -4.0)
	product := complexLeft * complexRight
	println(first, second, unsigned, signed, converted, right, "go"+"ml")
	println(negativeZero == zero, nan == nan, nan != nan, nan < zero)
	println(complexLeft+complexRight, product, real(product), imag(product))
}
