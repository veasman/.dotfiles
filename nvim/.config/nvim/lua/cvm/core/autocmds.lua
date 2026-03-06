local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local cvm_group = augroup("cvm", { clear = true })
local yank_group = augroup("HighlightYank", { clear = true })

autocmd("TextYankPost", {
    group = yank_group,
    callback = function()
        vim.highlight.on_yank({ higroup = "IncSearch", timeout = 120 })
    end,
})

autocmd("BufWritePre", {
    group = cvm_group,
    pattern = "*",
    callback = function()
        local save = vim.fn.winsaveview()
        vim.cmd([[silent! %s/\s\+$//e]])
        vim.fn.winrestview(save)
    end,
})
