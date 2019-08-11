const { sources, workspace } = require('coc.nvim')
const { spawn } = require('child_process')

exports.activate = async context => {
  context.subscriptions.push(
    sources.createSource({
      name: 'elm',
      filetypes: ['elm'],
      priority: 200,
      triggerCharacters: [],
      isSnippet: true,
      doComplete: async function(opt) {
        const content = workspace.getDocument(opt.bufnr).textDocument.getText();

        const elmCompletions = await query(context.logger, content, opt);

        return {
          items: elmCompletions.map(expr => {
            if (expr.includes("\n")) {
              const trimmedLines = [];
              expr.split("\n").forEach(line => {
                if (line.trim() !== "") {
                  trimmedLines.push(line.trim());
                }
              });

              const snippet = expr.split("\n");
              snippet.push("$0");

              return {
                word: expr.split("\n").join("\r"),
                abbr: `${expr.split("\n")[0].trim()}...`
              }
            } else {
              return {
                word: expr
              }
            }
          })
        };
      },
      onCompleteDone: async function() {
        await workspace.nvim.eval('feedkeys("jk:s/\\\\r/\\\\r/\\<CR>")');
      }
    })
  )
}



function query(logger, content, opt) {
  return new Promise((resolve, reject) => {
    logger.debug("query: ", opt.filepath);

    const elmAutocomplete = spawn("elm-autocomplete", [
      "--file",
      opt.filepath,
      "--column",
      `${opt.col + 2}`,
      "--row",
      `${opt.linenr}`
    ], {
    });

    const completions = [];

    elmAutocomplete.stdout.setEncoding("utf-8");
    elmAutocomplete.stdout.on("data", data => {
      const exprs = JSON.parse(data.toString());

      exprs.forEach(expr => {
        completions.push(expr)
      });
    });

    elmAutocomplete.on("close", data => {
      logger.debug("closing elm autocomplete: ", `${completions}`);
      resolve(completions);
    });

    elmAutocomplete.on("error", error => {
      reject(error);
    });

    elmAutocomplete.stdin.setEncoding("utf-8");
    elmAutocomplete.stdin.write(content);
    elmAutocomplete.stdin.end();
  });
}
