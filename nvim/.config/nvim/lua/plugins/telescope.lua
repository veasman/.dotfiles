local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
    return
end

--telescope.load_extension('media_files')

telescope.setup {
    defaults = {
        file_ignore_patterns = { "node%_modules/.*" },
    },
    pickers = {
        find_files = {
          --theme = "dropdown"
        }
        -- Default configuration for builtin pickers goes here:
        -- picker_name = {
        --   picker_config_key = value,
        --   ...
        -- }
        -- Now the picker_config_key will be applied every time you call this
        -- builtin picker
    },
    extensions = {
        --media_files = {
            --filetypes = {"png", "webp", "jpg", "jpeg"},
            --find_cmd = "rg"
        --},
    }
}
