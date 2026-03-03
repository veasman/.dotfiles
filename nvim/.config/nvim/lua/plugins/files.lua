return {
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Oil",
    keys = {
      { "-", "<cmd>Oil<cr>", desc = "Oil (file explorer)" },
    },
    opts = {
      view_options = { show_hidden = true },
    },
  },
}
