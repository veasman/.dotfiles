local neogit = require('neogit')

neogit.setup {}

vim.keymap.set('n', '<leader>g', vim.cmd.Neogit)
