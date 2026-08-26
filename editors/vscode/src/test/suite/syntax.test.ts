import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';

interface GrammarPattern {
    include?: string;
    match?: string;
}

interface GrammarRule {
    patterns: GrammarPattern[];
}

interface Grammar {
    patterns: GrammarPattern[];
    repository: Record<string, GrammarRule>;
}

suite('GoML Syntax Tests', () => {
    const grammarPath = path.resolve(__dirname, '../../../syntaxes/goml.tmLanguage.json');
    const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8')) as Grammar;

    function regex(rule: string, pattern: number): RegExp {
        const value = grammar.repository[rule].patterns[pattern].match;
        assert.ok(value, `Expected ${rule} pattern ${pattern} to define a match`);
        return new RegExp(value);
    }

    test('Loop labels take precedence over character literals', () => {
        const labels = grammar.patterns.findIndex(pattern => pattern.include === '#labels');
        const chars = grammar.patterns.findIndex(pattern => pattern.include === '#chars');
        assert.ok(labels >= 0);
        assert.ok(chars >= 0);
        assert.ok(labels < chars);

        const definition = regex('labels', 0);
        const reference = regex('labels', 1);
        assert.ok(definition.test("'outer: loop"));
        assert.ok(reference.test("break 'outer;"));
        assert.ok(reference.test("continue 'outer;"));
        assert.ok(!definition.test("'a'"));
        assert.ok(!definition.test("'ab'"));
    });

    test('Public imports and dyn associated bindings are recognized', () => {
        assert.ok(regex('declarations', 0).test('pub use client::Request as HttpRequest;'));
        assert.ok(regex('declarations', 0).test('use std::task;'));
        assert.ok(regex('associated-type-bindings', 0).test('dyn Iterator[Item = int]'));
    });

    test('Current declarations and builtin types are recognized', () => {
        const declarations = regex('declarations', 2);
        const keywords = regex('keywords', 4);
        const primitives = regex('types', 0);
        const builtins = regex('types', 1);
        assert.ok(declarations.test('static cache: OnceCell[string];'));
        assert.ok(keywords.test('static'));
        assert.ok(primitives.test('byte'));
        assert.ok(!primitives.test('unit'));
        assert.ok(builtins.test('MutSlice[byte]'));
        assert.ok(builtins.test('HashMapEntry[string, isize]'));
        assert.ok(builtins.test('OnceCell[string]'));
        assert.ok(builtins.test('FrozenVec[byte]'));
    });

    test('Task keywords remain contextual', () => {
        const contextual = regex('keywords', 0);
        assert.ok(contextual.test('scope {'));
        assert.ok(contextual.test('spawn |cancel| work(cancel)'));
        assert.ok(!contextual.test('let scope = 3;'));
        assert.ok(!contextual.test('spawn(scope)'));
    });

    test('Select keywords remain contextual', () => {
        const expressions = regex('keywords', 0);
        const arms = regex('keywords', 1);
        assert.ok(expressions.test('select {'));
        assert.ok(expressions.test('select priority {'));
        assert.ok(expressions.test('priority {'));
        assert.ok(arms.test('recv(input) as value => value'));
        assert.ok(arms.test('recv(input) when enabled as value => value'));
        assert.ok(arms.test('recv(input) when enabled match { Some(value) => value }'));
        assert.ok(arms.test('send(output, select(1)) => 2'));
        assert.ok(arms.test('send(output, value) when enabled => 2'));
        assert.ok(arms.test('default => 3'));
        assert.ok(!expressions.test('select(value)'));
        assert.ok(!arms.test('channel.recv()'));
        assert.ok(!arms.test('channel.send(value)'));
        assert.ok(!arms.test('let default = 3;'));
        assert.ok(!expressions.test('priority(value)'));
        assert.ok(!arms.test('let when = true;'));
    });
});
