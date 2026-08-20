package main

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"runtime"
)

type constantEvalRequest struct {
	Expression string `json:"expression"`
	GoVersion  string `json:"go_version"`
}

type constantEvalResponse struct {
	Mode        string                `json:"mode"`
	OK          bool                  `json:"ok"`
	GoVersion   string                `json:"go_version"`
	GOROOT      string                `json:"goroot"`
	Type        *normalizedType       `json:"type"`
	Value       *normalizedConstant   `json:"value"`
	Diagnostics []typeCheckDiagnostic `json:"diagnostics"`
}

func emptyConstantEvalResponse() constantEvalResponse {
	return constantEvalResponse{
		Mode: "constant-eval", OK: true, GoVersion: runtime.Version(), GOROOT: runtime.GOROOT(),
		Diagnostics: []typeCheckDiagnostic{},
	}
}

func constantEval(input []byte) constantEvalResponse {
	response := emptyConstantEvalResponse()
	var request constantEvalRequest
	if err := json.Unmarshal(input, &request); err != nil {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: err.Error()})
		return response
	}
	if request.Expression == "" {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: "expression must not be empty"})
		return response
	}
	if request.GoVersion == "" {
		request.GoVersion = "go1.26"
	}
	source := "package constantoracle\nconst result = (" + request.Expression + ")\n"
	fset := token.NewFileSet()
	file, parseError := parser.ParseFile(fset, "constant.go", source, parser.AllErrors|parser.SkipObjectResolution)
	response.Diagnostics = appendParseDiagnostics(response.Diagnostics, parseError)
	if file == nil {
		response.OK = false
		return response
	}
	info := &types.Info{Types: map[ast.Expr]types.TypeAndValue{}}
	config := &types.Config{GoVersion: request.GoVersion, Sizes: types.SizesFor("gc", "amd64")}
	config.Error = func(err error) { appendTypeDiagnostic(&response.Diagnostics, fset, err) }
	pkg, _ := config.Check("constantoracle", fset, []*ast.File{file}, info)
	declaration, ok := file.Decls[0].(*ast.GenDecl)
	if ok && len(declaration.Specs) > 0 {
		if spec, ok := declaration.Specs[0].(*ast.ValueSpec); ok && len(spec.Values) > 0 {
			if value, ok := info.Types[spec.Values[0]]; ok {
				normalizer := &typeNormalizer{fset: fset, packagePath: "constantoracle"}
				response.Type = normalizer.normalizeType(value.Type)
				response.Value = constantValue(value.Value)
			}
		}
	}
	response.OK = pkg != nil && len(response.Diagnostics) == 0 && response.Value != nil
	return response
}
