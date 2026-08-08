package main

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/constant"
	"go/importer"
	"go/parser"
	"go/scanner"
	"go/token"
	"go/types"
	"os"
	"reflect"
	"runtime"
	"sort"
	"strings"
)

type typeCheckFileInput struct {
	Name   string `json:"name"`
	Source string `json:"source"`
}

type typeCheckConfigInput struct {
	IgnoreFuncBodies         bool  `json:"ignore_func_bodies"`
	FakeImportC              bool  `json:"fake_import_c"`
	DisableUnusedImportCheck bool  `json:"disable_unused_import_check"`
	EnableAlias              bool  `json:"enable_alias"`
	CollectInfo              *bool `json:"collect_info"`
}

type typeCheckRequest struct {
	PackagePath string               `json:"package_path"`
	GoVersion   string               `json:"go_version"`
	GOOS        string               `json:"goos"`
	GOARCH      string               `json:"goarch"`
	Files       []typeCheckFileInput `json:"files"`
	Config      typeCheckConfigInput `json:"config"`
}

type typeCheckDiagnostic struct {
	File      string `json:"file"`
	Start     int    `json:"start"`
	End       int    `json:"end"`
	Message   string `json:"message"`
	Soft      bool   `json:"soft"`
	Secondary bool   `json:"secondary"`
}

type normalizedConstant struct {
	Kind  string `json:"kind"`
	Exact string `json:"exact"`
}

type normalizedTypeField struct {
	Name     string          `json:"name"`
	Package  string          `json:"package"`
	Embedded bool            `json:"embedded"`
	Tag      string          `json:"tag"`
	Type     *normalizedType `json:"type"`
}

type normalizedTypeVariable struct {
	Name    string          `json:"name"`
	Package string          `json:"package"`
	Type    *normalizedType `json:"type"`
}

type normalizedTypeMethod struct {
	Name    string          `json:"name"`
	Package string          `json:"package"`
	Object  string          `json:"object"`
	Type    *normalizedType `json:"type"`
}

type normalizedTypeTerm struct {
	Tilde bool            `json:"tilde"`
	Type  *normalizedType `json:"type"`
}

type normalizedTypeParameter struct {
	Name        string          `json:"name"`
	Declaration string          `json:"declaration"`
	Constraint  *normalizedType `json:"constraint"`
}

type normalizedType struct {
	Kind               string                    `json:"kind"`
	Name               string                    `json:"name"`
	Declaration        string                    `json:"declaration"`
	Text               string                    `json:"text"`
	Length             int64                     `json:"length"`
	Direction          string                    `json:"direction"`
	Variadic           bool                      `json:"variadic"`
	Comparable         bool                      `json:"comparable"`
	Implicit           bool                      `json:"implicit"`
	Elem               *normalizedType           `json:"elem"`
	Key                *normalizedType           `json:"key"`
	Receiver           *normalizedTypeVariable   `json:"receiver"`
	Fields             []normalizedTypeField     `json:"fields"`
	Parameters         []normalizedTypeVariable  `json:"parameters"`
	Results            []normalizedTypeVariable  `json:"results"`
	Methods            []normalizedTypeMethod    `json:"methods"`
	Embeddeds          []*normalizedType         `json:"embeddeds"`
	Terms              []normalizedTypeTerm      `json:"terms"`
	TypeParameters     []normalizedTypeParameter `json:"type_parameters"`
	ReceiverParameters []normalizedTypeParameter `json:"receiver_parameters"`
	TypeArguments      []*normalizedType         `json:"type_arguments"`
}

type normalizedObject struct {
	Key          string              `json:"key"`
	Kind         string              `json:"kind"`
	Name         string              `json:"name"`
	Package      string              `json:"package"`
	File         string              `json:"file"`
	Offset       int                 `json:"offset"`
	Exported     bool                `json:"exported"`
	Type         *normalizedType     `json:"type"`
	Value        *normalizedConstant `json:"value"`
	ImportedPath string              `json:"imported_path"`
}

type normalizedTypeFact struct {
	Node        string              `json:"node"`
	Type        *normalizedType     `json:"type"`
	Value       *normalizedConstant `json:"value"`
	Void        bool                `json:"void"`
	TypeExpr    bool                `json:"type_expr"`
	Builtin     bool                `json:"builtin"`
	ValueExpr   bool                `json:"value_expr"`
	Nil         bool                `json:"nil"`
	Addressable bool                `json:"addressable"`
	Assignable  bool                `json:"assignable"`
	CommaOK     bool                `json:"comma_ok"`
}

type normalizedObjectFact struct {
	Node   string            `json:"node"`
	Object *normalizedObject `json:"object"`
}

type normalizedSelection struct {
	Node     string            `json:"node"`
	Kind     string            `json:"kind"`
	Receiver *normalizedType   `json:"receiver"`
	Object   *normalizedObject `json:"object"`
	Type     *normalizedType   `json:"type"`
	Index    []int             `json:"index"`
	Indirect bool              `json:"indirect"`
}

type normalizedScope struct {
	Node        string             `json:"node"`
	File        string             `json:"file"`
	Start       int                `json:"start"`
	End         int                `json:"end"`
	ParentFile  string             `json:"parent_file"`
	ParentStart int                `json:"parent_start"`
	ParentEnd   int                `json:"parent_end"`
	Objects     []normalizedObject `json:"objects"`
}

type normalizedInstance struct {
	Node          string            `json:"node"`
	TypeArguments []*normalizedType `json:"type_arguments"`
	Type          *normalizedType   `json:"type"`
}

type normalizedInitializer struct {
	LHS []normalizedObject `json:"lhs"`
	RHS string             `json:"rhs"`
}

type normalizedFileVersion struct {
	File    string `json:"file"`
	Version string `json:"version"`
}

type normalizedPackage struct {
	Path     string `json:"path"`
	Name     string `json:"name"`
	Complete bool   `json:"complete"`
}

type typeCheckResponse struct {
	Mode          string                  `json:"mode"`
	OK            bool                    `json:"ok"`
	GoVersion     string                  `json:"go_version"`
	GOROOT        string                  `json:"goroot"`
	Package       normalizedPackage       `json:"package"`
	Diagnostics   []typeCheckDiagnostic   `json:"diagnostics"`
	UniverseScope []normalizedObject      `json:"universe_scope"`
	UnsafeScope   []normalizedObject      `json:"unsafe_scope"`
	PackageScope  []normalizedObject      `json:"package_scope"`
	Types         []normalizedTypeFact    `json:"types"`
	Defs          []normalizedObjectFact  `json:"defs"`
	Uses          []normalizedObjectFact  `json:"uses"`
	Implicits     []normalizedObjectFact  `json:"implicits"`
	Selections    []normalizedSelection   `json:"selections"`
	Scopes        []normalizedScope       `json:"scopes"`
	Instances     []normalizedInstance    `json:"instances"`
	InitOrder     []normalizedInitializer `json:"init_order"`
	FileVersions  []normalizedFileVersion `json:"file_versions"`
}

type typeNormalizer struct {
	fset        *token.FileSet
	packagePath string
	activeTypes map[types.Type]bool
	activeNamed map[*types.TypeName]bool
}

func emptyTypeCheckResponse() typeCheckResponse {
	return typeCheckResponse{
		Mode:          "type-check-package",
		OK:            true,
		GoVersion:     runtime.Version(),
		GOROOT:        runtime.GOROOT(),
		Diagnostics:   []typeCheckDiagnostic{},
		UniverseScope: []normalizedObject{},
		UnsafeScope:   []normalizedObject{},
		PackageScope:  []normalizedObject{},
		Types:         []normalizedTypeFact{},
		Defs:          []normalizedObjectFact{},
		Uses:          []normalizedObjectFact{},
		Implicits:     []normalizedObjectFact{},
		Selections:    []normalizedSelection{},
		Scopes:        []normalizedScope{},
		Instances:     []normalizedInstance{},
		InitOrder:     []normalizedInitializer{},
		FileVersions:  []normalizedFileVersion{},
	}
}

func emptyNormalizedType(kind string, value types.Type) *normalizedType {
	text := ""
	if value != nil {
		text = types.TypeString(value, func(pkg *types.Package) string { return pkg.Path() })
	}
	return &normalizedType{
		Kind:               kind,
		Text:               text,
		Fields:             []normalizedTypeField{},
		Parameters:         []normalizedTypeVariable{},
		Results:            []normalizedTypeVariable{},
		Methods:            []normalizedTypeMethod{},
		Embeddeds:          []*normalizedType{},
		Terms:              []normalizedTypeTerm{},
		TypeParameters:     []normalizedTypeParameter{},
		ReceiverParameters: []normalizedTypeParameter{},
		TypeArguments:      []*normalizedType{},
	}
}

func (normalizer *typeNormalizer) normalizeTypeReference(value types.Type) *normalizedType {
	switch typed := value.(type) {
	case *types.Named:
		result := emptyNormalizedType("Named", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
		return result
	case *types.Alias:
		result := emptyNormalizedType("Alias", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
		return result
	case *types.TypeParam:
		result := emptyNormalizedType("TypeParam", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
		return result
	default:
		return emptyNormalizedType(fmt.Sprintf("%T", value), value)
	}
}

func packagePath(pkg *types.Package) string {
	if pkg == nil {
		return ""
	}
	return pkg.Path()
}

func constantValue(value constant.Value) *normalizedConstant {
	if value == nil {
		return nil
	}
	return &normalizedConstant{Kind: value.Kind().String(), Exact: value.ExactString()}
}

func objectKind(value types.Object) string {
	switch value.(type) {
	case *types.PkgName:
		return "PackageName"
	case *types.Const:
		return "Const"
	case *types.TypeName:
		return "TypeName"
	case *types.Var:
		return "Var"
	case *types.Func:
		return "Func"
	case *types.Label:
		return "Label"
	case *types.Builtin:
		return "Builtin"
	case *types.Nil:
		return "Nil"
	default:
		return fmt.Sprintf("%T", value)
	}
}

func (normalizer *typeNormalizer) objectPosition(value types.Object) (string, int) {
	if !value.Pos().IsValid() {
		return "", -1
	}
	if value.Pkg() != nil && value.Pkg().Path() != normalizer.packagePath {
		return "<import>", -1
	}
	position := normalizer.fset.PositionFor(value.Pos(), false)
	if position.Filename == "" {
		return "", -1
	}
	return position.Filename, position.Offset
}

func (normalizer *typeNormalizer) objectKey(value types.Object) string {
	if value == nil {
		return ""
	}
	file, offset := normalizer.objectPosition(value)
	return fmt.Sprintf("%s|%s|%s|%s|%d", packagePath(value.Pkg()), objectKind(value), value.Name(), file, offset)
}

func (normalizer *typeNormalizer) normalizeObject(value types.Object) *normalizedObject {
	if value == nil {
		return nil
	}
	file, offset := normalizer.objectPosition(value)
	result := &normalizedObject{
		Key:      normalizer.objectKey(value),
		Kind:     objectKind(value),
		Name:     value.Name(),
		Package:  packagePath(value.Pkg()),
		File:     file,
		Offset:   offset,
		Exported: value.Exported(),
		Type:     normalizer.normalizeType(value.Type()),
	}
	if constantObject, ok := value.(*types.Const); ok {
		result.Value = constantValue(constantObject.Val())
	}
	if packageObject, ok := value.(*types.PkgName); ok && packageObject.Imported() != nil {
		result.ImportedPath = packageObject.Imported().Path()
	}
	return result
}

func (normalizer *typeNormalizer) normalizeVariable(value *types.Var) normalizedTypeVariable {
	if value == nil {
		return normalizedTypeVariable{}
	}
	return normalizedTypeVariable{Name: value.Name(), Package: packagePath(value.Pkg()), Type: normalizer.normalizeType(value.Type())}
}

func (normalizer *typeNormalizer) normalizeTuple(value *types.Tuple) []normalizedTypeVariable {
	result := []normalizedTypeVariable{}
	if value == nil {
		return result
	}
	for index := 0; index < value.Len(); index++ {
		result = append(result, normalizer.normalizeVariable(value.At(index)))
	}
	return result
}

func (normalizer *typeNormalizer) normalizeTypeList(value *types.TypeList) []*normalizedType {
	result := []*normalizedType{}
	if value == nil {
		return result
	}
	for index := 0; index < value.Len(); index++ {
		result = append(result, normalizer.normalizeType(value.At(index)))
	}
	return result
}

func (normalizer *typeNormalizer) normalizeTypeParameters(value *types.TypeParamList) []normalizedTypeParameter {
	result := []normalizedTypeParameter{}
	if value == nil {
		return result
	}
	for index := 0; index < value.Len(); index++ {
		parameter := value.At(index)
		result = append(result, normalizedTypeParameter{
			Name:        parameter.Obj().Name(),
			Declaration: normalizer.objectKey(parameter.Obj()),
			Constraint:  normalizer.normalizeType(parameter.Constraint()),
		})
	}
	return result
}

func (normalizer *typeNormalizer) normalizeType(value types.Type) (result *normalizedType) {
	result = emptyNormalizedType("Invalid", value)
	defer func() {
		if recover() != nil {
			result = emptyNormalizedType("Invalid", value)
		}
	}()
	if value == nil {
		return result
	}
	if normalizer.activeTypes == nil {
		normalizer.activeTypes = map[types.Type]bool{}
	}
	if normalizer.activeTypes[value] {
		return normalizer.normalizeTypeReference(value)
	}
	normalizer.activeTypes[value] = true
	defer delete(normalizer.activeTypes, value)
	switch typed := value.(type) {
	case *types.Basic:
		result = emptyNormalizedType("Basic", value)
		result.Name = typed.Name()
	case *types.Array:
		result = emptyNormalizedType("Array", value)
		result.Length = typed.Len()
		result.Elem = normalizer.normalizeType(typed.Elem())
	case *types.Slice:
		result = emptyNormalizedType("Slice", value)
		result.Elem = normalizer.normalizeType(typed.Elem())
	case *types.Struct:
		result = emptyNormalizedType("Struct", value)
		for index := 0; index < typed.NumFields(); index++ {
			field := typed.Field(index)
			result.Fields = append(result.Fields, normalizedTypeField{
				Name: field.Name(), Package: packagePath(field.Pkg()), Embedded: field.Embedded(),
				Tag: typed.Tag(index), Type: normalizer.normalizeType(field.Type()),
			})
		}
	case *types.Pointer:
		result = emptyNormalizedType("Pointer", value)
		result.Elem = normalizer.normalizeType(typed.Elem())
	case *types.Tuple:
		result = emptyNormalizedType("Tuple", value)
		result.Parameters = normalizer.normalizeTuple(typed)
	case *types.Signature:
		result = emptyNormalizedType("Signature", value)
		if typed.Recv() != nil {
			receiver := normalizer.normalizeVariable(typed.Recv())
			result.Receiver = &receiver
		}
		result.Parameters = normalizer.normalizeTuple(typed.Params())
		result.Results = normalizer.normalizeTuple(typed.Results())
		result.Variadic = typed.Variadic()
		result.TypeParameters = normalizer.normalizeTypeParameters(typed.TypeParams())
		result.ReceiverParameters = normalizer.normalizeTypeParameters(typed.RecvTypeParams())
	case *types.Interface:
		result = emptyNormalizedType("Interface", value)
		typed.Complete()
		result.Comparable = typed.IsComparable()
		result.Implicit = typed.IsImplicit()
		for index := 0; index < typed.NumExplicitMethods(); index++ {
			method := typed.ExplicitMethod(index)
			result.Methods = append(result.Methods, normalizedTypeMethod{
				Name: method.Name(), Package: packagePath(method.Pkg()), Object: normalizer.objectKey(method),
				Type: normalizer.normalizeType(method.Type()),
			})
		}
		for index := 0; index < typed.NumEmbeddeds(); index++ {
			result.Embeddeds = append(result.Embeddeds, normalizer.normalizeType(typed.EmbeddedType(index)))
		}
	case *types.Map:
		result = emptyNormalizedType("Map", value)
		result.Key = normalizer.normalizeType(typed.Key())
		result.Elem = normalizer.normalizeType(typed.Elem())
	case *types.Chan:
		result = emptyNormalizedType("Chan", value)
		result.Direction = channelDirection(typed.Dir())
		result.Elem = normalizer.normalizeType(typed.Elem())
	case *types.Named:
		if normalizer.activeNamed == nil {
			normalizer.activeNamed = map[*types.TypeName]bool{}
		}
		if normalizer.activeNamed[typed.Obj()] {
			return normalizer.normalizeTypeReference(value)
		}
		normalizer.activeNamed[typed.Obj()] = true
		defer delete(normalizer.activeNamed, typed.Obj())
		result = emptyNormalizedType("Named", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
		result.TypeArguments = normalizer.normalizeTypeList(typed.TypeArgs())
		result.TypeParameters = normalizer.normalizeTypeParameters(typed.TypeParams())
	case *types.Alias:
		result = emptyNormalizedType("Alias", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
		result.Elem = normalizer.normalizeType(typed.Rhs())
		result.TypeArguments = normalizer.normalizeTypeList(typed.TypeArgs())
		result.TypeParameters = normalizer.normalizeTypeParameters(typed.TypeParams())
	case *types.TypeParam:
		result = emptyNormalizedType("TypeParam", value)
		result.Name = typed.Obj().Name()
		result.Declaration = normalizer.objectKey(typed.Obj())
	case *types.Union:
		result = emptyNormalizedType("Union", value)
		for index := 0; index < typed.Len(); index++ {
			term := typed.Term(index)
			result.Terms = append(result.Terms, normalizedTypeTerm{Tilde: term.Tilde(), Type: normalizer.normalizeType(term.Type())})
		}
	default:
		result = emptyNormalizedType(fmt.Sprintf("%T", value), value)
	}
	return result
}

func astFieldName(name string) string {
	switch name {
	case "Tok":
		return "token"
	case "TokPos":
		return "token_pos"
	}
	var result strings.Builder
	for index, value := range name {
		if value >= 'A' && value <= 'Z' {
			if index > 0 {
				result.WriteByte('_')
			}
			result.WriteRune(value + ('a' - 'A'))
		} else {
			result.WriteRune(value)
		}
	}
	return result.String()
}

func astChild(value reflect.Value) (ast.Node, bool) {
	if !value.IsValid() || (value.Kind() == reflect.Pointer || value.Kind() == reflect.Interface) && value.IsNil() {
		return nil, false
	}
	if !value.CanInterface() {
		return nil, false
	}
	node, ok := value.Interface().(ast.Node)
	return node, ok && node != nil
}

func indexASTNode(paths map[ast.Node]string, node ast.Node, path string) {
	if node == nil {
		return
	}
	if _, exists := paths[node]; exists {
		return
	}
	paths[node] = path
	value := reflect.ValueOf(node)
	if value.Kind() != reflect.Pointer || value.IsNil() {
		return
	}
	value = value.Elem()
	valueType := value.Type()
	for index := 0; index < value.NumField(); index++ {
		fieldInfo := valueType.Field(index)
		if fieldInfo.Name == "Unresolved" {
			continue
		}
		field := value.Field(index)
		fieldPath := path + ".fields." + astFieldName(fieldInfo.Name)
		if child, ok := astChild(field); ok {
			indexASTNode(paths, child, fieldPath)
			continue
		}
		if field.Kind() != reflect.Slice || field.IsNil() {
			continue
		}
		for itemIndex := 0; itemIndex < field.Len(); itemIndex++ {
			if child, ok := astChild(field.Index(itemIndex)); ok {
				indexASTNode(paths, child, fmt.Sprintf("%s[%d]", fieldPath, itemIndex))
			}
		}
	}
}

func nodeKey(fset *token.FileSet, paths map[ast.Node]string, node ast.Node) string {
	if node == nil {
		return ""
	}
	file := fset.PositionFor(node.Pos(), false).Filename
	if path, ok := paths[node]; ok {
		return file + ":" + path
	}
	start := offsetOf(fset, node.Pos())
	end := offsetOf(fset, node.End())
	return fmt.Sprintf("%s:$unknown[%T:%d:%d]", file, node, start, end)
}

func diagnosticPosition(fset *token.FileSet, position token.Pos) (string, int) {
	if !position.IsValid() {
		return "", -1
	}
	value := fset.PositionFor(position, false)
	return value.Filename, value.Offset
}

func appendParseDiagnostics(target []typeCheckDiagnostic, parseError error) []typeCheckDiagnostic {
	if parseError == nil {
		return target
	}
	appendError := func(entry scanner.Error) {
		target = append(target, typeCheckDiagnostic{
			File: entry.Pos.Filename, Start: entry.Pos.Offset, End: entry.Pos.Offset,
			Message: entry.Msg,
		})
	}
	switch errors := parseError.(type) {
	case scanner.ErrorList:
		for _, entry := range errors {
			appendError(*entry)
		}
	case *scanner.ErrorList:
		for _, entry := range *errors {
			appendError(*entry)
		}
	default:
		target = append(target, typeCheckDiagnostic{Start: -1, End: -1, Message: parseError.Error()})
	}
	return target
}

func appendTypeDiagnostic(target *[]typeCheckDiagnostic, fset *token.FileSet, err error) {
	if typed, ok := err.(types.Error); ok {
		file, offset := diagnosticPosition(fset, typed.Pos)
		*target = append(*target, typeCheckDiagnostic{
			File: file, Start: offset, End: offset, Message: typed.Msg,
			Soft: typed.Soft, Secondary: strings.HasPrefix(typed.Msg, "\t"),
		})
		return
	}
	*target = append(*target, typeCheckDiagnostic{Start: -1, End: -1, Message: err.Error()})
}

func normalizedObjects(normalizer *typeNormalizer, scope *types.Scope) []normalizedObject {
	result := []normalizedObject{}
	if scope == nil {
		return result
	}
	for _, name := range scope.Names() {
		if object := normalizer.normalizeObject(scope.Lookup(name)); object != nil {
			result = append(result, *object)
		}
	}
	return result
}

func normalizeObjectFacts(normalizer *typeNormalizer, fset *token.FileSet, paths map[ast.Node]string, input map[*ast.Ident]types.Object) []normalizedObjectFact {
	result := []normalizedObjectFact{}
	for node, object := range input {
		result = append(result, normalizedObjectFact{Node: nodeKey(fset, paths, node), Object: normalizer.normalizeObject(object)})
	}
	sort.Slice(result, func(left, right int) bool { return result[left].Node < result[right].Node })
	return result
}

func normalizeImplicits(normalizer *typeNormalizer, fset *token.FileSet, paths map[ast.Node]string, input map[ast.Node]types.Object) []normalizedObjectFact {
	result := []normalizedObjectFact{}
	for node, object := range input {
		result = append(result, normalizedObjectFact{Node: nodeKey(fset, paths, node), Object: normalizer.normalizeObject(object)})
	}
	sort.Slice(result, func(left, right int) bool { return result[left].Node < result[right].Node })
	return result
}

func selectionKind(value types.SelectionKind) string {
	switch value {
	case types.FieldVal:
		return "FieldVal"
	case types.MethodVal:
		return "MethodVal"
	case types.MethodExpr:
		return "MethodExpr"
	default:
		return fmt.Sprintf("SelectionKind(%d)", value)
	}
}

func channelDirection(value types.ChanDir) string {
	switch value {
	case types.SendRecv:
		return "SendRecv"
	case types.SendOnly:
		return "SendOnly"
	case types.RecvOnly:
		return "RecvOnly"
	default:
		return fmt.Sprintf("ChanDir(%d)", value)
	}
}

func normalizeInfo(response *typeCheckResponse, fset *token.FileSet, files []*ast.File, paths map[ast.Node]string, pkg *types.Package, info *types.Info) {
	normalizer := &typeNormalizer{fset: fset, packagePath: pkg.Path()}
	response.Package = normalizedPackage{Path: pkg.Path(), Name: pkg.Name(), Complete: pkg.Complete()}
	response.UniverseScope = normalizedObjects(normalizer, types.Universe)
	response.UnsafeScope = normalizedObjects(normalizer, types.Unsafe.Scope())
	response.PackageScope = normalizedObjects(normalizer, pkg.Scope())
	for node, value := range info.Types {
		response.Types = append(response.Types, normalizedTypeFact{
			Node: nodeKey(fset, paths, node), Type: normalizer.normalizeType(value.Type), Value: constantValue(value.Value),
			Void: value.IsVoid(), TypeExpr: value.IsType(), Builtin: value.IsBuiltin(), ValueExpr: value.IsValue(),
			Nil: value.IsNil(), Addressable: value.Addressable(), Assignable: value.Assignable(), CommaOK: value.HasOk(),
		})
	}
	sort.Slice(response.Types, func(left, right int) bool { return response.Types[left].Node < response.Types[right].Node })
	response.Defs = normalizeObjectFacts(normalizer, fset, paths, info.Defs)
	response.Uses = normalizeObjectFacts(normalizer, fset, paths, info.Uses)
	response.Implicits = normalizeImplicits(normalizer, fset, paths, info.Implicits)
	for node, value := range info.Selections {
		response.Selections = append(response.Selections, normalizedSelection{
			Node: nodeKey(fset, paths, node), Kind: selectionKind(value.Kind()), Receiver: normalizer.normalizeType(value.Recv()),
			Object: normalizer.normalizeObject(value.Obj()), Type: normalizer.normalizeType(value.Type()),
			Index: append([]int{}, value.Index()...), Indirect: value.Indirect(),
		})
	}
	sort.Slice(response.Selections, func(left, right int) bool { return response.Selections[left].Node < response.Selections[right].Node })
	for node, value := range info.Scopes {
		file, start := diagnosticPosition(fset, value.Pos())
		_, end := diagnosticPosition(fset, value.End())
		parentFile := ""
		parentStart := -1
		parentEnd := -1
		if value.Parent() != nil {
			parentFile, parentStart = diagnosticPosition(fset, value.Parent().Pos())
			_, parentEnd = diagnosticPosition(fset, value.Parent().End())
		}
		response.Scopes = append(response.Scopes, normalizedScope{
			Node: nodeKey(fset, paths, node), File: file, Start: start, End: end,
			ParentFile: parentFile, ParentStart: parentStart, ParentEnd: parentEnd,
			Objects: normalizedObjects(normalizer, value),
		})
	}
	sort.Slice(response.Scopes, func(left, right int) bool { return response.Scopes[left].Node < response.Scopes[right].Node })
	for node, value := range info.Instances {
		response.Instances = append(response.Instances, normalizedInstance{
			Node: nodeKey(fset, paths, node), TypeArguments: normalizer.normalizeTypeList(value.TypeArgs),
			Type: normalizer.normalizeType(value.Type),
		})
	}
	sort.Slice(response.Instances, func(left, right int) bool { return response.Instances[left].Node < response.Instances[right].Node })
	for _, value := range info.InitOrder {
		initializer := normalizedInitializer{LHS: []normalizedObject{}, RHS: nodeKey(fset, paths, value.Rhs)}
		for _, object := range value.Lhs {
			if normalized := normalizer.normalizeObject(object); normalized != nil {
				initializer.LHS = append(initializer.LHS, *normalized)
			}
		}
		response.InitOrder = append(response.InitOrder, initializer)
	}
	for _, file := range files {
		response.FileVersions = append(response.FileVersions, normalizedFileVersion{
			File:    fset.PositionFor(file.Pos(), false).Filename,
			Version: info.FileVersions[file],
		})
	}
	sort.Slice(response.FileVersions, func(left, right int) bool {
		return response.FileVersions[left].File < response.FileVersions[right].File
	})
}

func validateTypeCheckRequest(request typeCheckRequest) error {
	if request.PackagePath == "" || request.PackagePath == "." {
		return fmt.Errorf("package_path must be a non-empty import path")
	}
	if len(request.Files) == 0 {
		return fmt.Errorf("files must contain at least one source file")
	}
	names := map[string]bool{}
	for _, file := range request.Files {
		if file.Name == "" {
			return fmt.Errorf("source file name must not be empty")
		}
		if names[file.Name] {
			return fmt.Errorf("duplicate source file name %q", file.Name)
		}
		names[file.Name] = true
	}
	return nil
}

func stabilizeTypeDiagnostics(diagnostics []typeCheckDiagnostic) {
	groups := [][]typeCheckDiagnostic{}
	slots := [][]int{}
	for i := 0; i < len(diagnostics); i++ {
		if strings.Contains(diagnostics[i].Message, " already declared through ") {
			group := []typeCheckDiagnostic{diagnostics[i]}
			groupSlots := []int{i}
			if i+1 < len(diagnostics) && diagnostics[i+1].Secondary {
				i++
				group = append(group, diagnostics[i])
				groupSlots = append(groupSlots, i)
			}
			groups = append(groups, group)
			slots = append(slots, groupSlots)
		}
	}
	if len(groups) < 2 {
		return
	}
	sort.SliceStable(groups, func(i, j int) bool {
		return groups[i][0].Start < groups[j][0].Start
	})
	for i, group := range groups {
		for j, diagnostic := range group {
			diagnostics[slots[i][j]] = diagnostic
		}
	}
}

func typeCheckPackage(input []byte) typeCheckResponse {
	response := emptyTypeCheckResponse()
	var request typeCheckRequest
	if err := json.Unmarshal(input, &request); err != nil {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: err.Error()})
		return response
	}
	if err := validateTypeCheckRequest(request); err != nil {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: err.Error()})
		return response
	}
	if request.GOARCH == "" {
		request.GOARCH = runtime.GOARCH
	}
	if request.GOOS == "" {
		request.GOOS = runtime.GOOS
	}
	if err := os.Setenv("GODEBUG", "gotypesalias="+map[bool]string{true: "1", false: "0"}[request.Config.EnableAlias]); err != nil {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: err.Error()})
		return response
	}
	sizes := types.SizesFor("gc", request.GOARCH)
	if sizes == nil {
		response.OK = false
		response.Diagnostics = append(response.Diagnostics, typeCheckDiagnostic{Start: -1, End: -1, Message: "unsupported GOARCH " + request.GOARCH})
		return response
	}
	fset := token.NewFileSet()
	files := []*ast.File{}
	paths := map[ast.Node]string{}
	for _, inputFile := range request.Files {
		file, err := parser.ParseFile(fset, inputFile.Name, inputFile.Source, parser.ParseComments|parser.AllErrors|parser.SkipObjectResolution)
		response.Diagnostics = appendParseDiagnostics(response.Diagnostics, err)
		if file != nil {
			files = append(files, file)
			indexASTNode(paths, file, "$")
		}
	}
	info := &types.Info{
		Types: map[ast.Expr]types.TypeAndValue{}, Instances: map[*ast.Ident]types.Instance{},
		Defs: map[*ast.Ident]types.Object{}, Uses: map[*ast.Ident]types.Object{},
		Implicits: map[ast.Node]types.Object{}, Selections: map[*ast.SelectorExpr]*types.Selection{},
		Scopes: map[ast.Node]*types.Scope{}, InitOrder: []*types.Initializer{}, FileVersions: map[*ast.File]string{},
	}
	config := &types.Config{
		GoVersion: request.GoVersion, IgnoreFuncBodies: request.Config.IgnoreFuncBodies,
		FakeImportC: request.Config.FakeImportC, DisableUnusedImportCheck: request.Config.DisableUnusedImportCheck,
		Importer: importer.Default(), Sizes: sizes,
	}
	config.Error = func(err error) { appendTypeDiagnostic(&response.Diagnostics, fset, err) }
	pkg, _ := config.Check(request.PackagePath, fset, files, info)
	stabilizeTypeDiagnostics(response.Diagnostics)
	collectInfo := request.Config.CollectInfo == nil || *request.Config.CollectInfo
	if pkg != nil && collectInfo {
		normalizeInfo(&response, fset, files, paths, pkg, info)
	}
	response.OK = len(response.Diagnostics) == 0
	return response
}
