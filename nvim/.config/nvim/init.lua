local data_site = vim.fn.stdpath("data") .. "/site"
vim.opt.runtimepath:prepend(data_site)

require("cvm.core")
require("cvm.lazy")
vim.cmd("LoomReloadTheme")
