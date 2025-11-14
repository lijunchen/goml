import Editor, { useMonaco } from '@monaco-editor/react';
import { useEffect, useMemo, useState } from 'react';
import { execute, compile_to_core, compile_to_mono, compile_to_go, compile_to_anf, hover, get_cst, get_ast, get_tast } from 'wasm-app';
import * as monacoEditor from 'monaco-editor/esm/vs/editor/editor.api';

const demos: Record<string, string> = {};

const loadDemos = async () => {
  const modules = import.meta.glob('../../crates/compiler/src/tests/pipeline/*.src', {
    query: '?raw',
    import: 'default'
  });

  const loadedEntries = await Promise.all(
    Object.entries(modules).map(async ([path, loader]) => {
      const name = path.split('/').pop()?.replace('.src', '') || 'unknown';
      const content = await loader() as string;
      return [name, content] as const;
    })
  );

  loadedEntries
    .sort(([a], [b]) => a.localeCompare(b))
    .forEach(([name, content]) => {
      demos[name] = content;
    });
};

type ViewMode = 'cst' | 'ast' | 'tast' | 'core' | 'mono' | 'anf' | 'go';
type PipelineOutputs = Record<ViewMode, string | null>;

const createEmptyPipelineOutputs = (): PipelineOutputs => ({
  cst: null,
  ast: null,
  tast: null,
  core: null,
  mono: null,
  anf: null,
  go: null
});

const formatError = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

const safeRun = (fn: (source: string) => string, source: string) => {
  try {
    return fn(source);
  } catch (error) {
    console.error(error);
    return `error: ${formatError(error)}`;
  }
};

function App() {
  const monaco = useMonaco();
  const [code, setCode] = useState("");
  const [result, setResult] = useState("");
  const [core, setCore] = useState("");
  const [selectedDemo, setSelectedDemo] = useState("");
  const [viewMode, setViewMode] = useState<ViewMode>('go');
  const [pipelineOutputs, setPipelineOutputs] = useState<PipelineOutputs>(createEmptyPipelineOutputs);

  const pipelineFns = useMemo(() => ({
    cst: get_cst,
    ast: get_ast,
    tast: get_tast,
    core: compile_to_core,
    mono: compile_to_mono,
    anf: compile_to_anf,
    go: compile_to_go
  }), []);

  useEffect(() => {
    loadDemos().then(() => {
      const demoNames = Object.keys(demos);
      const defaultDemo = demoNames[0];

      if (defaultDemo) {
        setSelectedDemo(defaultDemo);
        setCode(demos[defaultDemo]);
      }
    });
  }, []);

  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: 'simple' });

      monaco.languages.setMonarchTokensProvider('simple', {
        keywords: ['fn', 'let', 'in'],
        tokenizer: {
          root: [
            [/\b(fn|enum|trait|impl|for|match|if|else|let|in|return|true|false|unit|bool|int8|int16|int32|int64|uint8|uint16|uint32|uint64|string)\b/, "keyword"],
            [/\b[A-Z][a-zA-Z0-9_]*\b/, "type"],
            [/\b\d+\b/, "number"],
            [/[a-zA-Z_]\w*(?=\s*\()/, "function"],
            [/[a-zA-Z_]\w*/, "identifier"],
            [/[{}()\[\]]/, "@brackets"],
            [/[;,.]/, "delimiter"],
            [/".*?"/, "string"],
            [/\/\/.*/, "comment"],
          ],
        },
      });

      monaco.editor.defineTheme('simpleTheme', {
        base: 'vs',
        inherit: true,
        rules: [
          { token: 'keyword', foreground: '0000FF' },
          { token: 'type', foreground: '216C86' },
          { token: 'number', foreground: '09885A' },
          { token: 'identifier', foreground: '001080' },
          { token: 'string', foreground: 'A31515' },
          { token: 'function', foreground: '654D1D' },
        ],
        colors: {},
      });

      monaco.languages.registerHoverProvider('simple', {
        provideHover: async (
          model: monacoEditor.editor.ITextModel,
          position: monacoEditor.Position
        ): Promise<monacoEditor.languages.Hover | null> => {
          const line = position.lineNumber - 1;
          const col = position.column - 1;
          const content = model.getValue();
          const hoverText = hover(content, line, col);

          if (hoverText) {
            return {
              contents: [{ value: `\`\`\`simple\n${hoverText}\n\`\`\`` }],
            };
          }
          return null;
        },
      });

      monaco.editor.setTheme('simpleTheme');
    }
  }, [monaco]);

  useEffect(() => {
    setPipelineOutputs(createEmptyPipelineOutputs());
    setCore("");
    setResult(safeRun(execute, code));
  }, [code]);

  useEffect(() => {
    const output = pipelineOutputs[viewMode];
    if (output !== null) {
      setCore(output);
      return;
    }

    setCore("Loading...");

    const fn = pipelineFns[viewMode];
    const nextOutput = safeRun(fn, code);
    setPipelineOutputs(prev => ({
      ...prev,
      [viewMode]: nextOutput
    }));
  }, [code, pipelineFns, pipelineOutputs, viewMode]);

  const handleDemoChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const demoName = e.target.value;
    setSelectedDemo(demoName);
    const selectedCode = demos[demoName];

    if (selectedCode !== undefined) {
      setCode(selectedCode);
    }
  };

  const handleViewModeChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    setViewMode(e.target.value as ViewMode);
  };

  return (
    <div className="h-screen flex flex-col">
      <div className="bg-gray-100 p-2 flex items-center">
        <label className="mr-2 font-medium">Select Demo:</label>
        <select
          value={selectedDemo}
          onChange={handleDemoChange}
          className="border rounded p-1 mr-4"
        >
          {Object.keys(demos).map(demo => (
            <option key={demo} value={demo}>
              {demo.replace(/_/g, ' ')}
            </option>
          ))}
        </select>
        <label className="mr-2 font-medium">View Mode:</label>
        <select
          value={viewMode}
          onChange={handleViewModeChange}
          className="border rounded p-1"
        >
          <option value="cst">CST</option>
          <option value="ast">AST</option>
          <option value="tast">TAST</option>
          <option value="core">Core</option>
          <option value="mono">Mono</option>
          <option value="anf">ANF</option>
          <option value="go">Go</option>
        </select>
      </div>

      <div className="flex flex-1">
        <div className="w-1/2 border-r border-gray-300 flex flex-col">
          <Editor
            height="100%"
            language="simple"
            theme="simpleTheme"
            value={code}
            onChange={(value) => setCode(value || "")}
            options={{
              fontSize: 14,
              minimap: { enabled: false },
              automaticLayout: true,
              stickyScroll: {
                enabled: false
              }
            }}
          />
        </div>

        <div className="w-1/2 flex flex-col h-full min-h-0">
          <div className="flex-1 min-h-0 overflow-hidden p-4 flex flex-col">
            <h2 className="text-xl font-bold mb-2">{viewMode.toUpperCase()}</h2>
            <div className="flex-1 min-h-0">
              <Editor
                height="100%"
                language="plaintext"
                value={core}
                options={{
                  fontSize: 14,
                  minimap: { enabled: false },
                  readOnly: true,
                  stickyScroll: {
                    enabled: false
                  }
                }}
              />
            </div>
          </div>

          <div className="h-[20%] overflow-auto p-4 border-t border-gray-300">
            <h2 className="text-sm font-bold mb-1">Stdout</h2>
            <pre className="bg-gray-100 p-2 rounded whitespace-pre-wrap text-sm">
              {result}
            </pre>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;