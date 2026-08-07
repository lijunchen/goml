package main

import "testing"

var serdeBenchmarkSink int
var serdeBenchmarkJSONInput = _goml_m_benchmarks_p_serde_p_serde__benchmark__json__input()
var serdeBenchmarkBincodeStandardInput = _goml_m_benchmarks_p_serde_p_serde__benchmark__bincode__standard__input()
var serdeBenchmarkBincodeLegacyInput = _goml_m_benchmarks_p_serde_p_serde__benchmark__bincode__legacy__input()
var serdeBenchmarkTOMLInput = _goml_m_benchmarks_p_serde_p_serde__benchmark__toml__input()

func runSerdeBenchmark(b *testing.B, operation func() int, encodedSize int) {
	b.Helper()
	b.ReportAllocs()
	b.ResetTimer()
	b.ReportMetric(float64(encodedSize), "encoded-B")
	for index := 0; index < b.N; index++ {
		serdeBenchmarkSink = operation()
	}
}

func BenchmarkJSONDirectEncode(b *testing.B) {
	operation := _goml_m_benchmarks_p_serde_p_serde__benchmark__json__direct__encode
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkJSONValueEncode(b *testing.B) {
	operation := _goml_m_benchmarks_p_serde_p_serde__benchmark__json__value__encode
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkJSONDirectDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_serde__benchmark__json__direct__decode(serdeBenchmarkJSONInput)
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkJSONInput))
}

func BenchmarkJSONValueDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_serde__benchmark__json__value__decode(serdeBenchmarkJSONInput)
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkJSONInput))
}

func BenchmarkBincodeStandardDirectEncode(b *testing.B) {
	operation := _goml_m_benchmarks_p_serde_p_serde__benchmark__bincode__standard__direct__encode
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkBincodeStandardValueEncode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__value__encode(_goml_m_std_p_bincode_p_standard())
	}
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkBincodeStandardDirectDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__direct__decode(serdeBenchmarkBincodeStandardInput, _goml_m_std_p_bincode_p_standard())
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkBincodeStandardInput.items))
}

func BenchmarkBincodeStandardValueDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__value__decode(serdeBenchmarkBincodeStandardInput, _goml_m_std_p_bincode_p_standard())
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkBincodeStandardInput.items))
}

func BenchmarkBincodeLegacyDirectEncode(b *testing.B) {
	operation := _goml_m_benchmarks_p_serde_p_serde__benchmark__bincode__legacy__direct__encode
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkBincodeLegacyValueEncode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__value__encode(_goml_m_std_p_bincode_p_legacy())
	}
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkBincodeLegacyDirectDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__direct__decode(serdeBenchmarkBincodeLegacyInput, _goml_m_std_p_bincode_p_legacy())
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkBincodeLegacyInput.items))
}

func BenchmarkBincodeLegacyValueDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_bincode__value__decode(serdeBenchmarkBincodeLegacyInput, _goml_m_std_p_bincode_p_legacy())
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkBincodeLegacyInput.items))
}

func BenchmarkTOMLEncode(b *testing.B) {
	operation := _goml_m_benchmarks_p_serde_p_serde__benchmark__toml__encode
	runSerdeBenchmark(b, operation, operation())
}

func BenchmarkTOMLDecode(b *testing.B) {
	operation := func() int {
		return _goml_m_benchmarks_p_serde_p_toml__decode(serdeBenchmarkTOMLInput)
	}
	runSerdeBenchmark(b, operation, len(serdeBenchmarkTOMLInput))
}
