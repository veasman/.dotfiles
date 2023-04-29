require('rose-pine').setup({
	disable_background = true,
	disable_float_background = true,
	disable_italics = true,
})

require("gruvbox").setup({
    italic = {
        strings = false,
    },
    transparent_mode = true,
})


function ColorMyPencils(color)
	color = color or "rose-pine"
    vim.cmd.colorscheme(color)

	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

ColorMyPencils()
