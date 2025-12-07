const fs = require('fs');
const acorn = require('acorn');
const { SourceMapConsumer } = require('source-map');

async function main() {
  // read code and source map
  const code = fs.readFileSync('bundled_map.js', 'utf8');
  const rawSourceMap = JSON.parse(fs.readFileSync('bundled_map.js.map', 'utf8'));

  // parse AST with acorn
  const ast = acorn.parse(code, {
    ecmaVersion: 2020,
    sourceType: 'module',
    locations: true 
  });

  await SourceMapConsumer.with(rawSourceMap, null, (consumer) => {
    
    // inject the position information
    function enrichNode(node) {
      if (!node || typeof node !== 'object') return;

      if (node.loc) {
        const original = consumer.originalPositionFor({
          line: node.loc.start.line,
          column: node.loc.start.column
        });

        if (original.source) {
          node._source_loc = `${original.source}:${original.line}`;
        }
      }

      for (const key in node) {
        if (key === 'loc' || key === '_source_loc') continue;
        const child = node[key];
        if (Array.isArray(child)) {
          child.forEach(enrichNode);
        } else {
          enrichNode(child);
        }
      }
    }

    enrichNode(ast);
  });

  function cleanAST(key, value) {
    if (key === 'start' || key === 'end' || key === 'loc') return undefined;
    return value;
  }

  fs.writeFileSync('ast.json', JSON.stringify(ast, cleanAST, 2));
  console.log("ast.json generated with source maps embedded.");
}

main();