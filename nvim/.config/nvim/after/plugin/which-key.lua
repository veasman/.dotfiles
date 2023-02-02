require("which-key").setup({})

--local builtin = require('telescope.builtin')

--require("which-key").register({
    --["<leader>"] = { builtin.find_files, "Find file in project" },
    --s = {
        --name = "Search",
        --s = {
            --function()
                --builtin.grep_string({ search = vim.fn.input("Grep > ") })
            --end,
            --"Search project"
        --}
    --},
    --o = {
        --name = "Open",
        --p = {
            --function()
                --vim.nvim_command(":NvimTreeToggle")
            --end,
        --}
    --}
--}, { prefix = "<leader>" })
