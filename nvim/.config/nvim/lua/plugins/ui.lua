return {
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 900,
    opts = { style = "night" },
    config = function(_, opts)
      require("tokyonight").setup(opts)
      vim.cmd.colorscheme("tokyonight")
    end,
  },

  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = {
      options = { theme = "auto", section_separators = "", component_separators = "" },
    },
  },
}
