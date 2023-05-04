vim.keymap.set("n", "<leader>gs", vim.cmd.Git)

local Cvm_Fugitive = vim.api.nvim_create_augroup("Cvm_Fugitive", {})

local autocmd = vim.api.nvim_create_autocmd
autocmd("BufWinEnter", {
    group = Cvm_Fugitive,
    pattern = "*",
    callback = function()
        if vim.bo.ft ~= "fugitive" then
            return
        end

        local bufnr = vim.api.nvim_get_current_buf()
        local opts = {buffer = bufnr, remap = false}

        vim.keymap.set("n", "<leader>p", function()
            vim.cmd.Git('push')
        end, opts)

        vim.keymap.set("n", "<leader>c", function()
            local branch = vim.fn.trim(vim.fn.systemlist("git rev-parse --abbrev-ref HEAD"))[1]
            local commit_message = vim.fn.input("Commit message: ", branch .. ": ")
            vim.cmd("Git commit -m '" .. commit_message .. "' --edit")
        end, opts)


        -- rebase always
        vim.keymap.set("n", "<leader>P", function()
            vim.cmd.Git({'pull',  '--rebase'})
        end, opts)

        -- NOTE: It allows me to easily set the branch i am pushing and any tracking
        -- needed if i did not set the branch up correctly
        vim.keymap.set("n", "<leader>t", ":Git push -u origin ", opts);
    end,
})
