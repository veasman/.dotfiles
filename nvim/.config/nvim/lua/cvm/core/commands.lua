local M = {}

function M.reload_loom_theme()
    package.loaded["cvm.generated.theme"] = nil

    local ok, theme = pcall(require, "cvm.generated.theme")
    if not ok then
        vim.notify("Failed to load cvm.generated.theme", vim.log.levels.ERROR)
        return
    end

    if theme.colorscheme == "rose-pine" then
        require("rose-pine").setup({
            variant = theme.variant,
            disable_background = theme.transparent,
            styles = {
                italic = false,
            },
        })
    elseif theme.colorscheme == "gruvbox" then
        vim.o.background = theme.variant
    elseif theme.colorscheme == "material" then
        vim.g.material_style = theme.variant
    end

    vim.cmd.colorscheme(theme.colorscheme)

    vim.api.nvim_set_hl(0, "Normal", { bg = "NONE" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "NONE" })
end

vim.api.nvim_create_user_command("LoomReloadTheme", function()
    M.reload_loom_theme()
end, {})

-- auto cleanup sockets
vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        local server = vim.v.servername
        if server and server:match("^/tmp/nvim%-loom%-.*%.sock$") then
            pcall(vim.fn.delete, server)
        end
    end,
})

return M
