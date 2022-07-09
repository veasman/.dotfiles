local null_ls_status_ok, null_ls = pcall(require, 'null-ls')
if not null_ls_status_ok then
	return
end

-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/completion
---@diagnostic disable-next-line: unused-local
local completion = null_ls.builtins.completion
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/code_actions
local code_actions = null_ls.builtins.code_actions

null_ls.setup({
	debug = false,
	sources = {
		code_actions.eslint, -- typescript
		code_actions.shellcheck, -- shell
		-- diagnostics.cppcheck, -- cpp
		diagnostics.eslint, -- typescript
		diagnostics.gitlint, -- git commit messages
		diagnostics.hadolint, -- dockerfile
		diagnostics.markdownlint, -- markdown
		diagnostics.misspell, -- spelling
		diagnostics.shellcheck, -- shell
		diagnostics.yamllint, -- yaml
		-- formatting.clang_format, -- cpp
		formatting.jq, -- json
		formatting.prettier -- typescript
			.with({
				extra_args = { '--single-quote' }
			}),
		formatting.remark, -- markdown
		formatting.sqlfluff, -- sql
		formatting.stylelint, -- css
		formatting.stylua, -- lua
		formatting.tidy, -- html
	},
})
