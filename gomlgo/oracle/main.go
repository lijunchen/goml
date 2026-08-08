package main

import (
	"encoding/json"
	"fmt"
	"go/parser"
	"go/scanner"
	"go/token"
	"io"
	"os"
	"runtime"
	"sort"
	"unicode/utf8"
)

const sourceName = "<stdin>"

type protocolToken struct {
	Kind      string `json:"kind"`
	Literal   string `json:"literal"`
	Start     int    `json:"start"`
	End       int    `json:"end"`
	Synthetic bool   `json:"synthetic"`
}

type protocolDiagnostic struct {
	Message string `json:"message"`
	Offset  int    `json:"offset"`
}

type protocolResponse struct {
	Mode        string               `json:"mode"`
	OK          bool                 `json:"ok"`
	GoVersion   string               `json:"go_version"`
	GOROOT      string               `json:"goroot"`
	Tokens      []protocolToken      `json:"tokens"`
	AST         any                  `json:"ast"`
	Diagnostics []protocolDiagnostic `json:"diagnostics"`
}

func emptyResponse(mode string) protocolResponse {
	return protocolResponse{
		Mode:        mode,
		OK:          true,
		GoVersion:   runtime.Version(),
		GOROOT:      runtime.GOROOT(),
		Tokens:      []protocolToken{},
		AST:         map[string]any{},
		Diagnostics: []protocolDiagnostic{},
	}
}

func tokenEnd(source []byte, start int, kind token.Token, literal string, synthetic bool) int {
	if synthetic || kind == token.EOF {
		return start
	}
	if start < 0 || start >= len(source) {
		return start
	}
	if kind == token.COMMENT {
		if start+1 < len(source) && source[start+1] == '/' {
			end := start + 2
			for end < len(source) && source[end] != '\n' {
				end++
			}
			return end
		}
		end := start + 2
		for end+1 < len(source) && (source[end] != '*' || source[end+1] != '/') {
			end++
		}
		if end+1 < len(source) {
			return end + 2
		}
		return len(source)
	}
	if kind == token.STRING && source[start] == '`' {
		end := start + 1
		for end < len(source) && source[end] != '`' {
			end++
		}
		if end < len(source) {
			return end + 1
		}
		return len(source)
	}
	if kind == token.ILLEGAL {
		_, width := utf8.DecodeRune(source[start:])
		if width == 0 {
			width = 1
		}
		return start + width
	}
	end := start + len(literal)
	if end > len(source) {
		return len(source)
	}
	return end
}

func scanSource(source []byte) protocolResponse {
	response := emptyResponse("scan")
	fset := token.NewFileSet()
	file := fset.AddFile(sourceName, fset.Base(), len(source))
	var lexer scanner.Scanner
	lexer.Init(file, source, func(position token.Position, message string) {
		response.Diagnostics = append(response.Diagnostics, protocolDiagnostic{Message: message, Offset: position.Offset})
	}, scanner.ScanComments)
	for {
		position, kind, scannedLiteral := lexer.Scan()
		start := file.Offset(position)
		synthetic := kind == token.SEMICOLON && scannedLiteral != ";"
		literal := scannedLiteral
		if literal == "" && kind != token.EOF {
			literal = kind.String()
		}
		response.Tokens = append(response.Tokens, protocolToken{
			Kind:      kind.String(),
			Literal:   literal,
			Start:     start,
			End:       tokenEnd(source, start, kind, literal, synthetic),
			Synthetic: synthetic,
		})
		if kind == token.EOF {
			break
		}
	}
	sortDiagnostics(response.Diagnostics)
	response.OK = len(response.Diagnostics) == 0
	return response
}

func appendParserDiagnostics(target []protocolDiagnostic, parseError error) []protocolDiagnostic {
	if parseError == nil {
		return target
	}
	switch errors := parseError.(type) {
	case scanner.ErrorList:
		for _, entry := range errors {
			target = append(target, protocolDiagnostic{Message: entry.Msg, Offset: entry.Pos.Offset})
		}
	case *scanner.ErrorList:
		for _, entry := range *errors {
			target = append(target, protocolDiagnostic{Message: entry.Msg, Offset: entry.Pos.Offset})
		}
	default:
		target = append(target, protocolDiagnostic{Message: parseError.Error(), Offset: 0})
	}
	return target
}

func parseFileSource(source []byte) protocolResponse {
	response := emptyResponse("parse-file")
	fset := token.NewFileSet()
	file, parseError := parser.ParseFile(
		fset,
		sourceName,
		source,
		parser.ParseComments|parser.AllErrors|parser.SkipObjectResolution,
	)
	response.Diagnostics = appendParserDiagnostics(response.Diagnostics, parseError)
	sortDiagnostics(response.Diagnostics)
	if file != nil {
		response.AST = normalizeNode(fset, file)
	}
	response.OK = len(response.Diagnostics) == 0
	return response
}

func parseExprSource(source []byte) protocolResponse {
	response := emptyResponse("parse-expr")
	fset := token.NewFileSet()
	expression, parseError := parser.ParseExprFrom(fset, sourceName, source, parser.AllErrors)
	response.Diagnostics = appendParserDiagnostics(response.Diagnostics, parseError)
	sortDiagnostics(response.Diagnostics)
	if expression != nil {
		response.AST = normalizeNode(fset, expression)
	}
	response.OK = len(response.Diagnostics) == 0
	return response
}

func sortDiagnostics(diagnostics []protocolDiagnostic) {
	sort.SliceStable(diagnostics, func(left int, right int) bool {
		if diagnostics[left].Offset != diagnostics[right].Offset {
			return diagnostics[left].Offset < diagnostics[right].Offset
		}
		return diagnostics[left].Message < diagnostics[right].Message
	})
}

func execute(mode string, source []byte) protocolResponse {
	switch mode {
	case "version":
		return emptyResponse("version")
	case "scan":
		return scanSource(source)
	case "parse-file":
		return parseFileSource(source)
	case "parse-expr":
		return parseExprSource(source)
	default:
		response := emptyResponse(mode)
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, protocolDiagnostic{
			Message: fmt.Sprintf("unknown mode %q", mode),
			Offset:  0,
		})
		return response
	}
}

func executeProtocol(mode string, input []byte) any {
	switch mode {
	case "type-check-package":
		return typeCheckPackage(input)
	case "constant-eval":
		return constantEval(input)
	default:
		return execute(mode, input)
	}
}

func main() {
	mode := ""
	if len(os.Args) == 2 {
		mode = os.Args[1]
	}
	source, readError := io.ReadAll(os.Stdin)
	var response any = emptyResponse(mode)
	if readError != nil {
		failure := emptyResponse(mode)
		failure.OK = false
		failure.Diagnostics = append(failure.Diagnostics, protocolDiagnostic{Message: readError.Error(), Offset: 0})
		response = failure
	} else {
		response = executeProtocol(mode, source)
	}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetEscapeHTML(false)
	if encodeError := encoder.Encode(response); encodeError != nil {
		fmt.Fprintln(os.Stderr, encodeError)
		os.Exit(1)
	}
	if mode == "" || len(os.Args) != 2 {
		os.Exit(2)
	}
}
