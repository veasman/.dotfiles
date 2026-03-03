return {
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    priority = 1000,
    build = ":TSUpdate",
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
      ensure_installed = {
        "lua", "vim", "vimdoc",
        "bash", "json", "yaml", "toml",
        "dockerfile",
        "go", "gomod",
        "javascript", "typescript", "tsx",
        "markdown",
      },
      auto_install = true,
    },
    config = function(_, opts)
      local ok, configs = pcall(require, "nvim-treesitter.configs")
      if not ok then return end
      configs.setup(opts)
    end,
  },

  { "numToStr/Comment.nvim", event = "VeryLazy", opts = {} },
  { "kylechui/nvim-surround", event = "VeryLazy", opts = {} },
  { "windwp/nvim-autopairs", event = "InsertEnter", opts = {} },

  {
    "mbbill/undotree",
    keys = { { "<leader>u", "<cmd>UndotreeToggle<cr>", desc = "Undotree" } },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = { preset = "modern" },
  },

  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    opts = { use_diagnostic_signs = true },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix (Trouble)" },
    },
  },
}
