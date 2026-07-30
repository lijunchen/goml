const eslint = require('@eslint/js');
const globals = require('globals');
const typescriptEslint = require('typescript-eslint');

module.exports = typescriptEslint.config(
    {
        ignores: ['out/**'],
    },
    eslint.configs.recommended,
    typescriptEslint.configs.recommended,
    {
        files: ['src/**/*.ts'],
        languageOptions: {
            ecmaVersion: 2020,
            globals: globals.node,
            sourceType: 'module',
        },
    },
    {
        files: ['src/test/**/*.ts'],
        languageOptions: {
            globals: globals.mocha,
        },
    }
);
