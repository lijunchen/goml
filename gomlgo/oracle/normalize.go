package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"reflect"
)

type normalizedNode struct {
	Kind   string         `json:"kind"`
	Start  int            `json:"start"`
	End    int            `json:"end"`
	Fields map[string]any `json:"fields"`
}

func offsetOf(fset *token.FileSet, position token.Pos) int {
	if !position.IsValid() {
		return -1
	}
	return fset.PositionFor(position, false).Offset
}

func nodeStart(fset *token.FileSet, node ast.Node) (result int) {
	result = -1
	defer func() {
		if recover() != nil {
			result = -1
		}
	}()
	return offsetOf(fset, node.Pos())
}

func nodeEnd(fset *token.FileSet, node ast.Node) (result int) {
	result = -1
	defer func() {
		if recover() != nil {
			result = -1
		}
	}()
	return offsetOf(fset, node.End())
}

func normalizeSlice(fset *token.FileSet, value reflect.Value) any {
	if value.IsNil() {
		return nil
	}
	result := make([]any, value.Len())
	for index := 0; index < value.Len(); index++ {
		result[index] = normalizeValue(fset, value.Index(index).Interface())
	}
	return result
}

func normalizeValue(fset *token.FileSet, value any) any {
	if value == nil {
		return nil
	}
	reflected := reflect.ValueOf(value)
	if (reflected.Kind() == reflect.Pointer || reflected.Kind() == reflect.Interface || reflected.Kind() == reflect.Slice) && reflected.IsNil() {
		return nil
	}
	switch typed := value.(type) {
	case token.Pos:
		return offsetOf(fset, typed)
	case token.Token:
		return typed.String()
	case ast.ChanDir:
		return int(typed)
	case ast.Node:
		return normalizeNode(fset, typed)
	case string:
		return typed
	case bool:
		return typed
	case int:
		return typed
	}
	if reflected.Kind() == reflect.Slice {
		return normalizeSlice(fset, reflected)
	}
	return fmt.Sprintf("unsupported:%T", value)
}

func normalized(kind string, fset *token.FileSet, node ast.Node, fields map[string]any) normalizedNode {
	for name, value := range fields {
		fields[name] = normalizeValue(fset, value)
	}
	return normalizedNode{Kind: kind, Start: nodeStart(fset, node), End: nodeEnd(fset, node), Fields: fields}
}

func normalizeNode(fset *token.FileSet, node ast.Node) any {
	if node == nil {
		return nil
	}
	switch value := node.(type) {
	case *ast.Comment:
		return normalized("Comment", fset, value, map[string]any{"slash": value.Slash, "text": value.Text})
	case *ast.CommentGroup:
		return normalized("CommentGroup", fset, value, map[string]any{"list": value.List})
	case *ast.Field:
		return normalized("Field", fset, value, map[string]any{
			"doc": value.Doc, "names": value.Names, "type": value.Type, "tag": value.Tag, "comment": value.Comment,
		})
	case *ast.FieldList:
		return normalized("FieldList", fset, value, map[string]any{
			"opening": value.Opening, "list": value.List, "closing": value.Closing,
		})
	case *ast.BadExpr:
		return normalized("BadExpr", fset, value, map[string]any{"from": value.From, "to": value.To})
	case *ast.Ident:
		return normalized("Ident", fset, value, map[string]any{"name_pos": value.NamePos, "name": value.Name})
	case *ast.Ellipsis:
		return normalized("Ellipsis", fset, value, map[string]any{"ellipsis": value.Ellipsis, "elt": value.Elt})
	case *ast.BasicLit:
		return normalized("BasicLit", fset, value, map[string]any{
			"value_pos": value.ValuePos, "value_end": value.ValueEnd, "token": value.Kind, "value": value.Value,
		})
	case *ast.FuncLit:
		return normalized("FuncLit", fset, value, map[string]any{"type": value.Type, "body": value.Body})
	case *ast.CompositeLit:
		return normalized("CompositeLit", fset, value, map[string]any{
			"type": value.Type, "lbrace": value.Lbrace, "elts": value.Elts, "rbrace": value.Rbrace, "incomplete": value.Incomplete,
		})
	case *ast.ParenExpr:
		return normalized("ParenExpr", fset, value, map[string]any{"lparen": value.Lparen, "x": value.X, "rparen": value.Rparen})
	case *ast.SelectorExpr:
		return normalized("SelectorExpr", fset, value, map[string]any{"x": value.X, "sel": value.Sel})
	case *ast.IndexExpr:
		return normalized("IndexExpr", fset, value, map[string]any{
			"x": value.X, "lbrack": value.Lbrack, "index": value.Index, "rbrack": value.Rbrack,
		})
	case *ast.IndexListExpr:
		return normalized("IndexListExpr", fset, value, map[string]any{
			"x": value.X, "lbrack": value.Lbrack, "indices": value.Indices, "rbrack": value.Rbrack,
		})
	case *ast.SliceExpr:
		return normalized("SliceExpr", fset, value, map[string]any{
			"x": value.X, "lbrack": value.Lbrack, "low": value.Low, "high": value.High, "max": value.Max,
			"slice3": value.Slice3, "rbrack": value.Rbrack,
		})
	case *ast.TypeAssertExpr:
		return normalized("TypeAssertExpr", fset, value, map[string]any{
			"x": value.X, "lparen": value.Lparen, "type": value.Type, "rparen": value.Rparen,
		})
	case *ast.CallExpr:
		return normalized("CallExpr", fset, value, map[string]any{
			"fun": value.Fun, "lparen": value.Lparen, "args": value.Args, "ellipsis": value.Ellipsis, "rparen": value.Rparen,
		})
	case *ast.StarExpr:
		return normalized("StarExpr", fset, value, map[string]any{"star": value.Star, "x": value.X})
	case *ast.UnaryExpr:
		return normalized("UnaryExpr", fset, value, map[string]any{"op_pos": value.OpPos, "op": value.Op, "x": value.X})
	case *ast.BinaryExpr:
		return normalized("BinaryExpr", fset, value, map[string]any{
			"x": value.X, "op_pos": value.OpPos, "op": value.Op, "y": value.Y,
		})
	case *ast.KeyValueExpr:
		return normalized("KeyValueExpr", fset, value, map[string]any{"key": value.Key, "colon": value.Colon, "value": value.Value})
	case *ast.ArrayType:
		return normalized("ArrayType", fset, value, map[string]any{"lbrack": value.Lbrack, "len": value.Len, "elt": value.Elt})
	case *ast.StructType:
		return normalized("StructType", fset, value, map[string]any{
			"struct": value.Struct, "fields": value.Fields, "incomplete": value.Incomplete,
		})
	case *ast.FuncType:
		return normalized("FuncType", fset, value, map[string]any{
			"func": value.Func, "type_params": value.TypeParams, "params": value.Params, "results": value.Results,
		})
	case *ast.InterfaceType:
		return normalized("InterfaceType", fset, value, map[string]any{
			"interface": value.Interface, "methods": value.Methods, "incomplete": value.Incomplete,
		})
	case *ast.MapType:
		return normalized("MapType", fset, value, map[string]any{"map": value.Map, "key": value.Key, "value": value.Value})
	case *ast.ChanType:
		return normalized("ChanType", fset, value, map[string]any{
			"begin": value.Begin, "arrow": value.Arrow, "dir": value.Dir, "value": value.Value,
		})
	case *ast.BadStmt:
		return normalized("BadStmt", fset, value, map[string]any{"from": value.From, "to": value.To})
	case *ast.DeclStmt:
		return normalized("DeclStmt", fset, value, map[string]any{"decl": value.Decl})
	case *ast.EmptyStmt:
		return normalized("EmptyStmt", fset, value, map[string]any{"semicolon": value.Semicolon, "implicit": value.Implicit})
	case *ast.LabeledStmt:
		return normalized("LabeledStmt", fset, value, map[string]any{"label": value.Label, "colon": value.Colon, "stmt": value.Stmt})
	case *ast.ExprStmt:
		return normalized("ExprStmt", fset, value, map[string]any{"x": value.X})
	case *ast.SendStmt:
		return normalized("SendStmt", fset, value, map[string]any{"chan": value.Chan, "arrow": value.Arrow, "value": value.Value})
	case *ast.IncDecStmt:
		return normalized("IncDecStmt", fset, value, map[string]any{"x": value.X, "token_pos": value.TokPos, "token": value.Tok})
	case *ast.AssignStmt:
		return normalized("AssignStmt", fset, value, map[string]any{
			"lhs": value.Lhs, "token_pos": value.TokPos, "token": value.Tok, "rhs": value.Rhs,
		})
	case *ast.GoStmt:
		return normalized("GoStmt", fset, value, map[string]any{"go": value.Go, "call": value.Call})
	case *ast.DeferStmt:
		return normalized("DeferStmt", fset, value, map[string]any{"defer": value.Defer, "call": value.Call})
	case *ast.ReturnStmt:
		return normalized("ReturnStmt", fset, value, map[string]any{"return": value.Return, "results": value.Results})
	case *ast.BranchStmt:
		return normalized("BranchStmt", fset, value, map[string]any{
			"token_pos": value.TokPos, "token": value.Tok, "label": value.Label,
		})
	case *ast.BlockStmt:
		return normalized("BlockStmt", fset, value, map[string]any{"lbrace": value.Lbrace, "list": value.List, "rbrace": value.Rbrace})
	case *ast.IfStmt:
		return normalized("IfStmt", fset, value, map[string]any{
			"if": value.If, "init": value.Init, "cond": value.Cond, "body": value.Body, "else": value.Else,
		})
	case *ast.CaseClause:
		return normalized("CaseClause", fset, value, map[string]any{
			"case": value.Case, "list": value.List, "colon": value.Colon, "body": value.Body,
		})
	case *ast.SwitchStmt:
		return normalized("SwitchStmt", fset, value, map[string]any{
			"switch": value.Switch, "init": value.Init, "tag": value.Tag, "body": value.Body,
		})
	case *ast.TypeSwitchStmt:
		return normalized("TypeSwitchStmt", fset, value, map[string]any{
			"switch": value.Switch, "init": value.Init, "assign": value.Assign, "body": value.Body,
		})
	case *ast.CommClause:
		return normalized("CommClause", fset, value, map[string]any{
			"case": value.Case, "comm": value.Comm, "colon": value.Colon, "body": value.Body,
		})
	case *ast.SelectStmt:
		return normalized("SelectStmt", fset, value, map[string]any{"select": value.Select, "body": value.Body})
	case *ast.ForStmt:
		return normalized("ForStmt", fset, value, map[string]any{
			"for": value.For, "init": value.Init, "cond": value.Cond, "post": value.Post, "body": value.Body,
		})
	case *ast.RangeStmt:
		return normalized("RangeStmt", fset, value, map[string]any{
			"for": value.For, "key": value.Key, "value": value.Value, "token_pos": value.TokPos,
			"token": value.Tok, "range": value.Range, "x": value.X, "body": value.Body,
		})
	case *ast.ImportSpec:
		return normalized("ImportSpec", fset, value, map[string]any{
			"doc": value.Doc, "name": value.Name, "path": value.Path, "comment": value.Comment, "end_pos": value.EndPos,
		})
	case *ast.ValueSpec:
		return normalized("ValueSpec", fset, value, map[string]any{
			"doc": value.Doc, "names": value.Names, "type": value.Type, "values": value.Values, "comment": value.Comment,
		})
	case *ast.TypeSpec:
		return normalized("TypeSpec", fset, value, map[string]any{
			"doc": value.Doc, "name": value.Name, "type_params": value.TypeParams, "assign": value.Assign,
			"type": value.Type, "comment": value.Comment,
		})
	case *ast.BadDecl:
		return normalized("BadDecl", fset, value, map[string]any{"from": value.From, "to": value.To})
	case *ast.GenDecl:
		return normalized("GenDecl", fset, value, map[string]any{
			"doc": value.Doc, "token_pos": value.TokPos, "token": value.Tok, "lparen": value.Lparen,
			"specs": value.Specs, "rparen": value.Rparen,
		})
	case *ast.FuncDecl:
		return normalized("FuncDecl", fset, value, map[string]any{
			"doc": value.Doc, "recv": value.Recv, "name": value.Name, "type": value.Type, "body": value.Body,
		})
	case *ast.File:
		return normalized("File", fset, value, map[string]any{
			"doc": value.Doc, "package": value.Package, "name": value.Name, "decls": value.Decls,
			"file_start": value.FileStart, "file_end": value.FileEnd, "imports": value.Imports,
			"comments": value.Comments, "go_version": value.GoVersion,
		})
	default:
		return normalized("Unsupported", fset, node, map[string]any{"go_type": fmt.Sprintf("%T", node)})
	}
}
