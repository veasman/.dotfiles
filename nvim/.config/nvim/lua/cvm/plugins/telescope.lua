return {
    "nvim-telescope/telescope.nvim",
    version = "0.2.0",
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    cmd = "Telescope",
    keys = {
        {
            "<leader>pf",
            function()
                require("telescope.builtin").find_files({ hidden = true, follow = true })
            end,
            desc = "Find files",
        },
        {
            "<leader>ps",
            function()
                require("telescope.builtin").live_grep()
            end,
            desc = "Live grep",
        },
        {
            "<leader>pr",
            function()
                require("telescope.builtin").oldfiles()
            end,
            desc = "Recent files",
        },
    },
    config = function()
        local telescope = require("telescope")
        local actions = require("telescope.actions")
        local builtin = require("telescope.builtin")

        telescope.setup({
            defaults = {
                path_display = { "smart" },
                file_ignore_patterns = {
                    "node_modules",
                    ".git/",
                    "dist/",
                    "build/",
                    "target/",
                    "%.lock",
                },
                mappings = {
                    i = {
                        ["<C-j>"] = actions.move_selection_next,
                        ["<C-k>"] = actions.move_selection_previous,
                        ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
                    },
                },
            },
            pickers = {
                find_files = {
                    hidden = true,
                    follow = true,
                },
                live_grep = {
                    additional_args = function()
                        return { "--hidden" }
                    end,
                },
            },
        })

        pcall(telescope.load_extension, "fzf")

        vim.api.nvim_create_user_command("Files", function()
            builtin.find_files({ hidden = true, follow = true })
        end, {})

        vim.api.nvim_create_user_command("Grep", function(opts)
            builtin.live_grep({ default_text = opts.args })
        end, { nargs = "*" })
    end,
}
