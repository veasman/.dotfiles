local M = {}

local function apply_transparency(enabled)
    local bg = enabled and "NONE" or nil

    vim.api.nvim_set_hl(0, "Normal", { bg = bg })
    vim.api.nvim_set_hl(0, "NormalNC", { bg = bg })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = bg })
    vim.api.nvim_set_hl(0, "FloatBorder", { bg = bg })
    vim.api.nvim_set_hl(0, "SignColumn", { bg = bg })
    vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = bg })
end

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
        if theme.variant == "light" or theme.variant == "dark" then
            vim.o.background = theme.variant
        else
            vim.o.background = "dark"
        end
    elseif theme.colorscheme == "material" then
        vim.g.material_style = theme.variant
    elseif theme.colorscheme == "catppuccin" then
        require("catppuccin").setup({
            flavour = theme.variant,
            transparent_background = theme.transparent,
            no_italic = true,
        })
    end

    vim.cmd.colorscheme(theme.colorscheme)

    vim.schedule(function()
        apply_transparency(theme.transparent)
    end)
end

vim.api.nvim_create_user_command("LoomReloadTheme", function()
    M.reload_loom_theme()
end, {})

vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        local server = vim.v.servername
        if server and server:match("^/tmp/nvim%-loom%-.*%.sock$") then
            pcall(vim.fn.delete, server)
        end
    end,
})

return M
