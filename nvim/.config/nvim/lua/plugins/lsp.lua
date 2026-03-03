return {
  { "mason-org/mason.nvim", cmd = "Mason", opts = { ui = { border = "rounded" } } },

  { "mason-org/mason-lspconfig.nvim", dependencies = { "mason-org/mason.nvim" } },

  {
    "neovim/nvim-lspconfig",
    dependencies = { "mason-org/mason-lspconfig.nvim", "hrsh7th/cmp-nvim-lsp" },
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      require("mason").setup()

      local servers = {
        lua_ls = { settings = { Lua = { diagnostics = { globals = { "vim" } } } } },
        gopls = {},
        bashls = {},
        jsonls = {},
        yamlls = {},
        dockerls = {},
        ts_ls = {},
        eslint = {},
      }

      require("mason-lspconfig").setup({ ensure_installed = vim.tbl_keys(servers) })

      local on_attach = function(_, bufnr)
        local map = function(mode, lhs, rhs, desc)
          vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
        end
        map("n", "gd", vim.lsp.buf.definition, "Go to definition")
        map("n", "K", vim.lsp.buf.hover, "Hover")
        map("n", "gr", vim.lsp.buf.references, "References")
        map("n", "<leader>rn", vim.lsp.buf.rename, "Rename")
        map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
      end

      for name, cfg in pairs(servers) do
        cfg.capabilities = capabilities
        cfg.on_attach = on_attach
        vim.lsp.config(name, cfg)
      end
      vim.lsp.enable(vim.tbl_keys(servers))
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
    },
    opts = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")

      return {
        snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
        mapping = cmp.mapping.preset.insert({
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "path" },
          { name = "buffer" },
        }),
      }
    end,
  },

  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    opts = function()
      vim.g.format_on_save = true
      return {
        format_on_save = function(bufnr)
          if not vim.g.format_on_save then return end
          local ft = vim.bo[bufnr].filetype
          local allowed = {
            lua = true, go = true,
            javascript = true, typescript = true, tsx = true,
            json = true, yaml = true, dockerfile = true,
            bash = true, sh = true,
          }
          if not allowed[ft] then return end
          return { timeout_ms = 2000, lsp_fallback = true }
        end,
        formatters_by_ft = {
          lua = { "stylua" },
          go = { "gofmt", "goimports" },
          javascript = { "prettier" },
          typescript = { "prettier" },
          tsx = { "prettier" },
          json = { "prettier" },
          yaml = { "prettier" },
          dockerfile = { "prettier" },
          sh = { "shfmt" },
        },
      }
    end,
  },
}
